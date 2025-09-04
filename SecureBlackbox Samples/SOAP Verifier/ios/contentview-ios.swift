import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

func signValidRes(value: SignatureValidities) -> String {
    switch (value) {
        case SignatureValidities.svtValid: return "Valid"
        case SignatureValidities.svtCorrupted: return "Corrupted"
        case SignatureValidities.svtSignerNotFound: return "SignerNotFound"
        case SignatureValidities.svtFailure: return "Failure"
        case SignatureValidities.svtReferenceCorrupted: return "ReferenceCorrupted"
        default: return "Unknown"
    }
}

func signChainRes(value: ChainValidities) -> String {
    switch (value) {
        case ChainValidities.cvtValid: return "Valid"
        case ChainValidities.cvtValidButUntrusted: return "Valid but untrusted"
        case ChainValidities.cvtCantBeEstablished: return "Can't be established"
        case ChainValidities.cvtInvalid: return "Invalid"
        default: return "Unknown"
    }
}

struct ContentView: View, SOAPVerifierDelegate {

    var verifier = SOAPVerifier()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
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
            }

            do {
                try verifier.verify()
                  
                var verifyRes = ""
                var i = 0
                for signature in verifier.signatures {
                    i+=1
                    verifyRes += "Signature# \(i) \n"
                    verifyRes += " Subject RDN: \(signature.subjectRDN) \n"
                    verifyRes += " Timestamp: \(signature.claimedSigningTime) \n"
                    verifyRes += " Signature Validation Result: \(signValidRes(value: signature.signatureValidationResult)) \n"
                    verifyRes += " Chain Validation Result: \(signChainRes(value: signature.chainValidationResult)) \n"
                }

                showMessage = ShowMes(message: verifyRes, title: "Info")
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
    func onRetrieveCertificate(uri: String) {}
    func onRetrieveCRL(uri: String) {}
    func onRetrieveOCSPResponse(uri: String) {}
    
    func onSignatureFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {
        validateChain = false // disabling certificate chain validation for demo only
        
        if (certFound) {
            validateSignature = true
        } else {
            showMessage = ShowMes(message: "Certificate not found")
        }
    }
    
    func onSignatureValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onStoreCertificate(cert: Data, uri: inout String) {}
    func onStoreCRL(crl: Data, uri: inout String) {}
    func onStoreOCSPResponse(ocspResponse: Data, uri: inout String) {}
    func onTimestampFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateTimestamp: inout Bool, validateChain: inout Bool) {}
    func onTimestampValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, time: String, validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32) {}
    func onTLSCertValidate(serverHost: String, serverIP: String, accept: inout Bool) {}
    func onChainElementDownload(kind: Int32, certRDN: String, caCertRDN: String, location: String, action: inout Int32) {}
    func onChainElementNeeded(kind: Int32, certRDN: String, caCertRDN: String) {}
    func onChainElementStore(kind: Int32, body: Data, uri: inout String) {}
    func onChainValidated(index: Int32, subjectRDN: String, validationResult: Int32, validationDetails: Int32) {}
    func onChainValidationProgress(eventKind: String, certRDN: String, caCertRDN: String, action: inout Int32) {}
    func onMessageLoaded(cancel: inout Bool) {}
    func onReferenceValidated(referenceIndex: Int32, id: String, uri: String, refType: String, digestValid: Bool) {}
    func onResolveReference(referenceIndex: Int32, uri: String) {}
    func onSignatureFound(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {}
    func onSignatureValidated(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onTimestampFound(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateTimestamp: inout Bool, validateChain: inout Bool) {}
    func onTimestampValidated(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, time: String, validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32) {}
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
