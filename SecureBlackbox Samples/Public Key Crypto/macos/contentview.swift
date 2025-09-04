import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var crypto = PublicKeyCrypto()
    var certmanager = CertificateManager()
    var keymanager = CryptoKeyManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var operationType = 0
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var encodingType = 0
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var showMessage: ShowMes?
       
    func outputLabel() -> String {
        if (operationType == 1) {
            return "Signature file:"
        } else {
            return "Output file:"
        }
    }
    
    func actionLabel() -> String {
        if (operationType == 1) {
            return "Verify"
        } else {
            return "Sign"
        }
    }
    
    var body: some View {
        VStack(alignment: .center) {
            Group {
                Text("Signature type:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Picker(selection: $operationType, label: Text("")) {
                    Text("Sign").tag(0)
                    Text("Verify").tag(1)
                }
                .pickerStyle(SegmentedPickerStyle())
                
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            
                Text("Input file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $inputFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openInputButton()
                }
                
                
                Text("\(outputLabel())")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $outputFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                opensaveOutputButton()
                }
                
                
                Text("Encoding type:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Picker(selection: $encodingType, label: Text("")) {
                    Text("Binary")
                        .tag(0)
                    Text("Base64")
                        .tag(1)
                    Text("Compact")
                        .tag(2)
                    Text("JSON")
                        .tag(3)
                }
                .pickerStyle(SegmentedPickerStyle())
                
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            }
            Text("Certificate file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $certFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openCertButton()
            }
            
            
            Text("Certificate password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            actionButton()
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
    private func opensaveOutputButton() -> some View {
        Button(action: {
            var dialog: NSSavePanel
            if (operationType == 1) {
                dialog = NSOpenPanel()
                dialog.title = "Choose an signature file"
                dialog.showsResizeIndicator = true
                dialog.showsHiddenFiles = false
                (dialog as! NSOpenPanel).allowsMultipleSelection = true
                (dialog as! NSOpenPanel).canChooseDirectories = true
            } else {
                dialog = NSSavePanel()
                dialog.title = "Choose an output file"
                dialog.showsResizeIndicator = true
                dialog.showsHiddenFiles = false
            }

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
    private func openCertButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title                   = "Choose an certificate file"
            dialog.showsResizeIndicator    = true
            dialog.showsHiddenFiles        = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false
            dialog.allowedFileTypes        = ["cer", "pem", "pfx"]

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    certFile = result!.path
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
    private func actionButton() -> some View {
        Button(action: {
            //crypto.runtimeLicense = ""
            showMessage = nil
            do {
                // key from certificate
                try certmanager.importFromFile(path: certFile, password: certPass)
                keymanager.certificate = certmanager.certificate
                try keymanager.importFromCert()
                crypto.key = keymanager.key
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
            }
            
            do {
                if (operationType == 0) {
                    crypto.inputEncoding = PublickeycryptoInputEncodings.cetBinary
                    switch (encodingType) {
                        case 1:
                            crypto.outputEncoding = PublickeycryptoOutputEncodings.cetBase64
                            break
                        case 2:
                            crypto.outputEncoding = PublickeycryptoOutputEncodings.cetCompact
                            break
                        case 3:
                            crypto.outputEncoding = PublickeycryptoOutputEncodings.cetJSON
                            break
                        default:
                            crypto.outputEncoding = PublickeycryptoOutputEncodings.cetBinary
                            break
                    }
                    try crypto.signFile(sourceFile: inputFile, destFile: outputFile, detached: true)
                    showMessage = ShowMes(message: "The file successfully signed", title: "Info")
                } else {
                    switch (encodingType) {
                        case 1:
                            crypto.inputEncoding =  PublickeycryptoInputEncodings.cetBase64
                            break
                        case 2:
                            crypto.inputEncoding =  PublickeycryptoInputEncodings.cetCompact
                            break
                        case 3:
                            crypto.inputEncoding =  PublickeycryptoInputEncodings.cetJSON
                            break
                        default:
                            crypto.inputEncoding =  PublickeycryptoInputEncodings.cetBinary
                            break
                    }
                    crypto.outputEncoding =  PublickeycryptoOutputEncodings.cetBinary;
                    try crypto.verifyDetachedFile(signedDataFile: inputFile, signatureFile: outputFile)
                    switch (crypto.signatureValidationResult) {
                        case PublickeycryptoSignatureValidationResults.svtValid:
                            showMessage = ShowMes(message: "Signature validated successfully", title: "Info")
                            break
                        case PublickeycryptoSignatureValidationResults.svtCorrupted:
                            showMessage = ShowMes(message: "Signature is invalid", title: "Info")
                            break
                        case PublickeycryptoSignatureValidationResults.svtSignerNotFound:
                            showMessage = ShowMes(message: "Signer not found", title: "Info")
                            break
                        case PublickeycryptoSignatureValidationResults.svtFailure:
                            showMessage = ShowMes(message: "Signature verification failed", title: "Info")
                            break
                        default:
                            showMessage = ShowMes(message: "Unknown", title: "Info")
                    }
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("\(actionLabel())").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
