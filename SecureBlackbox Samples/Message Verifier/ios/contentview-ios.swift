import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, MessageVerifierDelegate {

    var verifier = MessageVerifier()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var detached = false
    @State private var outpurFileLabel: String = "Output file:"
    @State private var outputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var showMessage: ShowMes?
                
    func detachedChange() -> String {
        if (detached) {
            return "Data file:"
        } else {
            return "Output file:"
        }
    }
    
    var body: some View {
        VStack(alignment: .center) {
            Group
            {
                Text("Input file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $inputFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                
                
                Toggle(isOn: $detached) {
                    Text("Detached")
                }
                
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
              
                Text("\(detachedChange())")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $outputFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                
            }
            Text("Certificate file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $certFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            
            
            Text("Password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            verifyButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func verifyButton() -> some View {
        Button(action: {
            //verifier.runtimeLicense = ""
            verifier.delegate = self
            showMessage = nil
            verifier.inputFile = documentsPath + inputFile
            let outputPath = documentsPath + outputFile
            let certPath = documentsPath + certFile

            if (certFile != "") {
                do {
                    try certmanager.importFromFile(path: certPath, password: certPass)
                    verifier.knownCertificates.append(certmanager.certificate)
                } catch {
                    showMessage = ShowMes(message: "Error \(error)")
                    return
                }
            }
           
            do {
                if (detached) {
                    verifier.dataFile = outputPath
                    try verifier.verifyDetached()
                } else {
                    verifier.outputFile = outputPath
                    try verifier.verify()
                }
            
                switch (verifier.signatureValidationResult) {
                    case MessageverifierSignatureValidationResults.svtValid:
                        showMessage = ShowMes(message: "Signature validated successfully", title: "Info")
                        break
                    case MessageverifierSignatureValidationResults.svtCorrupted:
                        showMessage = ShowMes(message: "Signature is invalid", title: "Info")
                        break
                    case MessageverifierSignatureValidationResults.svtSignerNotFound:
                        showMessage = ShowMes(message: "Signer not found", title: "Info")
                        break
                    case MessageverifierSignatureValidationResults.svtFailure:
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
    
    func onError(errorCode: Int32, description: String) {}
    func onNotification(eventID: String, eventParam: String) {}
    
    func onSignatureFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {
        validateChain = false // disabling certificate chain validation.
        
        if (certFound) {
            validateSignature = true
        } else {
            showMessage = ShowMes(message: "Certificate not found")
        }
    }
    
    func onSignatureValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onTimestampFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateTimestamp: inout Bool, validateChain: inout Bool) {}
    func onTimestampValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, time: String, validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32) {}
    func onRecipientFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
