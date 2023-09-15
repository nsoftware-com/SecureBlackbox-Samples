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
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                
                
                Text("\(outputLabel())")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $outputFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(.none)
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
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(.none)
            }
            
            
            Text("Certificate password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            actionButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func actionButton() -> some View {
        Button(action: {
            //crypto.runtimeLicense = ""
            showMessage = nil
            do {
                // key from certificate
                try certmanager.importFromFile(path: documentsPath + certFile, password: certPass)
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
                    try crypto.signFile(sourceFile: documentsPath + inputFile, destFile: documentsPath + outputFile, detached: true)
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
                    try crypto.verifyDetachedFile(signedDataFile: documentsPath + inputFile, signatureFile: documentsPath + outputFile)
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
