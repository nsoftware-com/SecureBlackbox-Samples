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
    
    @State private var compact = false
    @State private var inputStr: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var signatureStr: String = ""
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Plaintext string:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $inputStr)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
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
                
            Toggle(isOn: $compact) {
                Text("Compact")
            }
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                        
            Text("Signature:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $signatureStr)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
                signButton()
                verifyButton()
            }
            
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
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
    private func signButton() -> some View {
        Button(action: {
            //crypto.runtimeLicense = ""
            showMessage = nil
            signatureStr = ""
            crypto.inputEncoding = PublickeycryptoInputEncodings.cetBinary
            if (compact) {
                crypto.outputEncoding = PublickeycryptoOutputEncodings.cetCompact
            } else {
                crypto.outputEncoding = PublickeycryptoOutputEncodings.cetJSON
            }
            
            do {
                let certPath = certFile
                // key from certificate
                try certmanager.importFromFile(path: certPath, password: certPass)
                keymanager.certificate = certmanager.certificate
                try keymanager.importFromCert()
                crypto.key = keymanager.key
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
         
            do {
                let inBuf = inputStr.data(using: .utf8)
                let outBuf = try crypto.sign(buffer: inBuf!, detached:  true)
                signatureStr = String(decoding: outBuf, as: UTF8.self)
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Sign").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func verifyButton() -> some View {
        Button(action: {
            //crypto.runtimeLicense = ""
            showMessage = nil
            
            do {
                if (compact) {
                    crypto.inputEncoding = PublickeycryptoInputEncodings.cetCompact
                } else {
                    crypto.inputEncoding = PublickeycryptoInputEncodings.cetJSON
                }
                crypto.outputEncoding = PublickeycryptoOutputEncodings.cetBinary
                
                do {
                    let certPath = certFile
                    // key from certificate
                    try certmanager.importFromFile(path: certPath, password: certPass)
                    keymanager.certificate = certmanager.certificate
                    try keymanager.importFromCert()
                    crypto.key = keymanager.key
                } catch {
                    showMessage = ShowMes(message: "Error \(error)")
                    return
                }
                
                let inBuf = inputStr.data(using: .utf8)
                let sigBuf = signatureStr.data(using: .utf8)
                try crypto.verifyDetached(signedData: inBuf!, signature: sigBuf!)
                            
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
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Verify").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
