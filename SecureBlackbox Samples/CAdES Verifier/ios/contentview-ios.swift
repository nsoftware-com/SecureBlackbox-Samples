import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, CAdESVerifierDelegate {

    var verifier = CAdESVerifier()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var detached = false
    @State private var outpurFileLabel: String = "Output file:"
    @State private var outputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var showMessage: ShowMes?
        
    func signValidRes(value: SignatureValidities) -> String {
        switch (value) {
            case SignatureValidities.svtValid: return "Signature validated successfully"
            case SignatureValidities.svtCorrupted: return "Signature is invalid"
            case SignatureValidities.svtSignerNotFound: return "Signer not found"
            case SignatureValidities.svtFailure: return "Signature verification failed"
            default: return "Unknown"
        }
    }
        
    func detachedChange() -> String {
        if (detached) {
            return "Data file:"
        } else {
            return "Output file:"
        }
    }
    
    var body: some View {
        VStack(alignment: .center) {
            Group {
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
                showMessage = ShowMes(message: signValidRes(value: verifier.signature.signatureValidationResult), title: "Info")
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Verify").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    func onChainValidated(subjectRDN: String, validationResult: Int32, validationDetails: Int32) {}
    func onCountersignatureFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {}
    func onCountersignatureValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onError(errorCode: Int32, description: String) {}
    func onNotification(eventID: String, eventParam: String) {}
    
    func onSignatureFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {
        if (certFound) {
            validateSignature = true
            validateChain = true
        } else {
            showMessage = ShowMes(message: "Certificate not found");
        }
    }
    
    func onSignatureValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onTimestampFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateTimestamp: inout Bool, validateChain: inout Bool) {}
    func onTimestampValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, time: String, validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32) {}
    func onTLSCertValidate(serverHost: String, serverIP: String, accept: inout Bool) {}
    func onChainElementDownload(kind: Int32, certRDN: String, caCertRDN: String, location: String, action: inout Int32) {}
    func onChainElementNeeded(kind: Int32, certRDN: String, caCertRDN: String) {}
    func onChainValidationProgress(eventKind: String, certRDN: String, caCertRDN: String, action: inout Int32) {}
    func onSignatureFound(scope: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, last: Bool, validateSignature: inout Bool, validateChain: inout Bool) {}
    func onSignatureProcessed(validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32, last: Bool, proceed: inout Bool) {}
    func onSignatureValidated(validationResult: Int32, proceed: inout Bool) {}
    func onTimestampFound(timestampType: Int32, time: String, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, last: Bool, validateTimestamp: inout Bool, validateChain: inout Bool) {}
    func onTimestampProcessed(validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32, last: Bool, proceed: inout Bool) {}
    func onTimestampValidated(validationResult: Int32, proceed: inout Bool) {}
    func onTLSCertNeeded(host: String, caNames: String) {}
    func onTLSEstablished(host: String, version: String, ciphersuite: String, connectionId: Data, abort: inout Bool) {}
    func onTLSHandshake(host: String, abort: inout Bool) {}
    func onTLSShutdown(host: String) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
