import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, XMLVerifierDelegate {

    var verifier = XMLVerifier()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var detached = false
    @State private var dataFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var references: String = ""
    @State private var showMessage: ShowMes?
    
    var body: some View {
        VStack(alignment: .center) {
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
                
            if (detached) {
                Text("Data file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $dataFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                
            }
            Group {
                Text("Certificate file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $certFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                
                
                Text("Password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $certPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            verifyButton()
            TextEditor(text: $references)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                .border(Color.black, width: 1)
                
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
            references = ""
            verifier.inputFile = documentsPath + inputFile
            let certPath = documentsPath + certFile
            if (certFile != "") {
                do {
                    try certmanager.importFromFile(path: certPath, password: certPass)
                    verifier.knownCertificates.append(certmanager.certificate)
                } catch {
                    showMessage = ShowMes(message: "Error \(error)")
                    return
                }
            } else {
                while verifier.knownCertificates.count > 0 {
                    verifier.knownCertificates.remove(at: 0)
                }
            }

            do {
                if (detached) {
                    verifier.dataFile = documentsPath + dataFile
                    verifier.dataType = XmlverifierDataTypes.cxdtBinary
                    verifier.dataURI = (dataFile as NSString).lastPathComponent

                    try verifier.verifyDetached()
                } else {
                    try verifier.verify()
                }
                
                for signature in verifier.signatures {                    
                    switch (signature.signatureValidationResult) {
                        case XMLSignatureValidities.xsvValid:
                            showMessage = ShowMes(message: "Signature validated successfully.", title: "Info")
                            break
                        case XMLSignatureValidities.xsvCorrupted:
                            showMessage = ShowMes(message: "Signature is invalid", title: "Info")
                            break
                        case XMLSignatureValidities.xsvSignerNotFound:
                            showMessage = ShowMes(message: "Signer not found", title: "Info")
                            break
                        case XMLSignatureValidities.xsvFailure:
                            showMessage = ShowMes(message: "Signature verification failed", title: "Info")
                            break
                        case XMLSignatureValidities.xsvReferenceCorrupted:
                            showMessage = ShowMes(message: "Signature has invalid references.", title: "Info")
                            break
                        default:
                            showMessage = ShowMes(message: "Unknown signature validation result", title: "Info")
                    }
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
    
    func onReferenceValidated(id: String, uri: String, refType: String, referenceIndex: Int32, digestValid: Bool) {
        references += "Reference# \(referenceIndex + 1) \n"
        references += "   Id: \(id) \n"
        references += "   URI: \(uri) \n"
        references += "   Ref type: \(refType) \n"
        if (digestValid) {
            references += "   Valid: true \n"
        } else {
            references += "   Valid: false \n"
        }
    }
    
    func onResolveReference(uri: String, referenceIndex: Int32) {}
    
    func onSignatureFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {
        validateChain = false // disabling certificate chain validation for test only
        
        if (certFound) {
            validateSignature = true
        } else {
            showMessage = ShowMes(message: "Certificate not found")
        }
    }
    
    func onSignatureValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onDocumentLoaded(cancel: inout Bool) {}
    func onReferenceValidated(referenceIndex: Int32, id: String, uri: String, refType: String, digestValid: Bool) {}
    func onResolveReference(referenceIndex: Int32, uri: String) {}
    func onSignatureFound(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {}
    func onSignatureValidated(sigIndex: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
