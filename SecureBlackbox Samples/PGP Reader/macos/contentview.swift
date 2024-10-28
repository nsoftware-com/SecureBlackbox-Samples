import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var reader = PGPReader()
    var keyring = PGPKeyring()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    func userFriendlyKeyName(username : String, keyId:String, subKey:Bool) -> String {
        var res = "    [PRI]"
        if (subKey) {
            res = "    [SUB]"
        }
        
        if (username.count > 0) {
            res += username
        } else {
            res += "No name"
        }
        
        return res + " [0x\(keyId)]"
    }

    func decKeyName(row: Int) -> String {
        if (row == 0) {
            return "     <select encryption key>"
        } else {
            let secretKey = keyring.secretKeys[row - 1]
            return userFriendlyKeyName(username: secretKey.username, keyId: secretKey.keyID, subKey:  secretKey.isSubkey)
        }
    }

    func verifyKeyName(row: Int) -> String {
        if (row == 0) {
            return "     <select signing key>"
        } else {
            let publicKey = keyring.publicKeys[row - 1]
            return userFriendlyKeyName(username: publicKey.username, keyId:  publicKey.keyID, subKey: publicKey.isSubkey)
        }
    }
    
    func signValidRes(value: PGPSignatureValidities) -> String {
        switch (value) {
            case PGPSignatureValidities.psvValid: return "Valid"
            case PGPSignatureValidities.psvCorrupted: return "Corrupted"
            case PGPSignatureValidities.psvUnknownAlgorithm: return "Unknown signing algorithm"
            case PGPSignatureValidities.psvNoKey: return "Signing key not found, unable to verify"
            default: return "Unknown"
        }
    }

    func showSignatures() {
        showVerifyRes = true
        verifyRes = ""
        var i = 0
        for signature in reader.signatures {
            i += 1
            verifyRes += "Signature# \(i) \n"
            verifyRes += " Signer KeyID: \(signature.signerKeyID) \n"
            verifyRes += " Hash algorithm: \(signature.hashAlgorithm) \n"
            verifyRes += " Creation Time: \(signature.creationTime) \n"
            verifyRes += " Signature Validation Result: \(signValidRes(value: signature.validity)) \n"
        }
    }
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var pubFile: String = ""
    @State private var secFile: String = ""
    @State private var keysLoading = false
    @State private var autoSelect = false
    @State private var selectedDecKey: Int = 0
    @State private var selectedVerKey: Int = 0
    @State private var keyPass: String = ""
    @State private var showVerifyRes = false
    @State private var verifyRes: String = ""
    @State private var showMessage: ShowMes?
    
    private let keyringPickerStyle = PopUpButtonPickerStyle()
    
    var body: some View {
        VStack(alignment: .center) {
            Text("Input file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $inputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openInputButton()
            }
            
            
            Text("Output file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $outputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            saveOutputButton()
            }
            

            if (!keysLoading && !showVerifyRes) {
                Text("Public keyring:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $pubFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openPubkeyButton()
                }
                
                
                Text("Secret keyring:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $secFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openSeckeyButton()
                }
                
                
                loadButton()
            } else if (!showVerifyRes) {
                Toggle(isOn: $autoSelect) {
                    Text("Automatically select appropriate keys")
                }
                
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                Text("Key list for decryption:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Section {
                    Picker("", selection: $selectedDecKey) {
                        ForEach((0...keyring.secretKeys.count), id: \.self) { Text("\(decKeyName(row: $0))").tag($0) }
                    }
                    .pickerStyle(keyringPickerStyle)
                    .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
                }
                .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
                
                .disabled(autoSelect == true)
                
                Text("Key for verifying:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Section {
                    Picker("", selection: $selectedVerKey) {
                        ForEach((0...keyring.publicKeys.count), id: \.self) { Text("\(verifyKeyName(row: $0))").tag($0) }
                    }
                    .pickerStyle(keyringPickerStyle)
                    .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
                }
                .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
                
                .disabled(autoSelect == true)
                
                Text("Password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $keyPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                    backButton()
                    decverButton()
                }
            } else {
                Text("Signatures:")
                TextEditor(text: $verifyRes)
                    .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                    .border(Color.black, width: 1)
                    
                closeButton()
            }
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func openInputButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title = "Choose an input file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    inputFile = result!.path
                }
            }
        }, label: {
            Text("...").font(.system(size: 18))
                .frame(minWidth: 20, minHeight: 20)
                .background(RoundedRectangle(cornerRadius: 2).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
        
    @ViewBuilder
    private func saveOutputButton() -> some View {
        Button(action: {
            let dialog = NSSavePanel()
            dialog.title = "Choose an output file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    outputFile = result!.path
                }
            }
        }, label: {
            Text("...").font(.system(size: 18))
                .frame(minWidth: 20, minHeight: 20)
                .background(RoundedRectangle(cornerRadius: 2).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
        
    @ViewBuilder
    private func openPubkeyButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title                   = "Choose an public key file"
            dialog.showsResizeIndicator    = true
            dialog.showsHiddenFiles        = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    pubFile = result!.path
                }
            }
        }, label: {
            Text("...").font(.system(size: 18))
                .frame(minWidth: 20, minHeight: 20)
                .background(RoundedRectangle(cornerRadius: 2).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }

    @ViewBuilder
    private func openSeckeyButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title                   = "Choose an secret key file"
            dialog.showsResizeIndicator    = true
            dialog.showsHiddenFiles        = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    secFile = result!.path
                }
            }
        }, label: {
            Text("...").font(.system(size: 18))
                .frame(minWidth: 20, minHeight: 20)
                .background(RoundedRectangle(cornerRadius: 2).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }

    @ViewBuilder
    private func loadButton() -> some View {
        Button(action: {
            //keyring.runtimeLicense = ""
            showMessage = nil
            do {
                if (keyring.opened) {
                    try keyring.close()
                }
                try keyring.load(publicKeyringFile: pubFile, secretKeyringFile: secFile)
                autoSelect = true
                selectedDecKey = 0
                selectedVerKey = 0
                keysLoading = true
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Load").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func backButton() -> some View {
        Button(action: {
            showMessage = nil
            do {
                keysLoading = false
                try keyring.close()
            } catch {
                showMessage = ShowMes(message: "Failed to list mailboxes on the server. \(error)")
                return
            }
        }, label: {
            Text("Back").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func decverButton() -> some View {
        Button(action: {
            showMessage = nil
            do {
                if (keyring.secretKeys.count == 0) {
                    showMessage = ShowMes(message: "Your keyring does not contain private keys. You will not be able to decrypt encrypted files. \nPlease, select another keyring file.")
                } else if (keyring.publicKeys.count == 0) {
                    showMessage = ShowMes(message: "Your keyring does not contain public keys. You will not be able to verify files. \nPlease, select another keyring file.")
                } else if (!autoSelect && selectedDecKey <= 0) {
                    showMessage = ShowMes(message: "Please select the decryption key")
                } else if (!autoSelect && selectedVerKey <= 0) {
                    showMessage = ShowMes(message: "Please select the verifing key")
                } else {
                    // clear keys arrays
                    while reader.verifyingKeys.count > 0 {
                        reader.verifyingKeys.remove(at: 0)
                    }
                    while reader.decryptingKeys.count > 0 {
                        reader.decryptingKeys.remove(at: 0)
                    }
                    
                    if (autoSelect) {
                        for pubkey in keyring.publicKeys {
                            reader.verifyingKeys.append(pubkey)
                        }
                        for seckey in keyring.secretKeys {
                            reader.decryptingKeys.append(seckey)
                        }
                    } else {
                        reader.verifyingKeys.append(keyring.publicKeys[selectedDecKey - 1])
                        reader.decryptingKeys.append(keyring.secretKeys[selectedVerKey - 1])
                    }
                           
                    reader.keyPassphrase = keyPass
                    reader.inputFile = inputFile
                    reader.outputFile = outputFile
                    try reader.decryptAndVerify()
                    
                    showSignatures()
                    showMessage = ShowMes(message: "The file was decrypted successfully", title: "Info")
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Decrypt and verify").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func closeButton() -> some View {
        Button(action: {
            showVerifyRes = false
        }, label: {
            Text("Close").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
