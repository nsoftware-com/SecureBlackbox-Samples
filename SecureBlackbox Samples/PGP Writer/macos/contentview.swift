import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var writer = PGPWriter()
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

    func encKeyName(row: Int) -> String {
        if (row == 0) {
            return "     <select encryption key>"
        } else {
            let publicKey = keyring.publicKeys[row - 1]
            return userFriendlyKeyName(username: publicKey.username, keyId: publicKey.keyID, subKey: publicKey.isSubkey)
        }
    }

    func signKeyName(row: Int) -> String {
        if (row == 0) {
            return "     <select signing key>"
        } else {
            let secretKey = keyring.secretKeys[row - 1]
            return userFriendlyKeyName(username: secretKey.username, keyId: secretKey.keyID, subKey: secretKey.isSubkey)
        }
    }
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var pubFile: String = ""
    @State private var secFile: String = ""
    @State private var keysLoading = false
    @State private var selectedEncKey: Int = 0
    @State private var selectedSignKey: Int = 0
    @State private var keyPass: String = ""
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
            
            
            if (!keysLoading) {
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
            } else {
                Text("Please select the encryption key:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Section {
                    Picker("", selection: $selectedEncKey) {
                        ForEach((0...keyring.publicKeys.count), id: \.self) { Text("\(encKeyName(row: $0))").tag($0) }
                    }
                    .pickerStyle(keyringPickerStyle)
                    .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
                }
                .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
                
                
                Text("Please select the signing key:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Section {
                    Picker("", selection: $selectedSignKey) {
                        ForEach((0...keyring.secretKeys.count), id: \.self) { Text("\(signKeyName(row: $0))").tag($0) }
                    }
                    .pickerStyle(keyringPickerStyle)
                    .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
                }
                .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
                
                
                Text("Password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $keyPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                    backButton()
                    encsignButton()
                }
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
                selectedEncKey = 0
                selectedSignKey = 0
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
    private func encsignButton() -> some View {
        Button(action: {
            showMessage = nil
            do {
                if (keyring.secretKeys.count == 0) {
                    showMessage = ShowMes(message: "Your keyring does not contain private keys. You will not be able to encrypt file. \nPlease, select another keyring file.")
                } else if (keyring.publicKeys.count == 0) {
                    showMessage = ShowMes(message: "Your keyring does not contain public keys. You will not be able to sign file. \nPlease, select another keyring file.")
                } else if (selectedEncKey <= 0) {
                    showMessage = ShowMes(message: "Please select the encryption key")
                } else if (selectedSignKey <= 0) {
                    showMessage = ShowMes(message: "Please select the signing key")
                } else {
                    writer.armor = true
                    writer.armorHeaders = "Version: OpenPGPBlackbox"
                    writer.armorBoundary = "PGP MESSAGE"
                    
                    while writer.encryptingKeys.count > 0 {
                        writer.encryptingKeys.remove(at: 0)
                    }
                    writer.encryptingKeys.append(keyring.publicKeys[selectedEncKey - 1])
                    
                    while writer.signingKeys.count > 0 {
                        writer.signingKeys.remove(at: 0)
                    }
                    writer.signingKeys.append(keyring.secretKeys[selectedSignKey])
                    
                    writer.signingKeys[0].passphrase = keyPass
                    writer.filename = inputFile
                    writer.inputFile = inputFile
                    writer.outputFile = outputFile
                    try writer.encryptAndSign()
                    
                    showMessage = ShowMes(message: "The file were encrypted and signed successfully", title: "Info")
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Encrypt and sign").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
