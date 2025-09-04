import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

func signSignatureType(value: PDFSignatureTypes) -> String {
    switch (value) {
        case PDFSignatureTypes.pstLegacy: return "Legacy"
        case PDFSignatureTypes.pstPAdES: return "PAdES";
        case PDFSignatureTypes.pstDocumentTimestamp: return "Document Timestamp";
        default: return "Unknown"
    }
}

func signLevel(value: PAdESSignatureLevels) -> String {
    switch (value) {
        case PAdESSignatureLevels.paslGeneric: return "Generic/Legacy"
        case PAdESSignatureLevels.paslBaselineB: return "BaselineB";
        case PAdESSignatureLevels.paslBaselineT: return "BaselineT";
        case PAdESSignatureLevels.paslBaselineLT: return "BaselineLT";
        case PAdESSignatureLevels.paslBaselineLTA: return "BaselineLTA";
        case PAdESSignatureLevels.paslBES: return "BES"
        case PAdESSignatureLevels.paslEPES: return "EPES"
        case PAdESSignatureLevels.paslLTV: return "LTV"
        default: return "Unknown"
    }
}

func signValidRes(value: SignatureValidities) -> String {
    switch (value) {
        case SignatureValidities.svtValid: return "Valid"
        case SignatureValidities.svtCorrupted: return "Corrupted"
        case SignatureValidities.svtSignerNotFound: return "SignerNotFound"
        case SignatureValidities.svtFailure: return "Failure"
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

struct ContentView: View, PDFVerifierDelegate {
    var verifier = PDFVerifier()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var verifyRes: String = ""
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
                
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            verifyButton()
            TextEditor(text: $verifyRes)
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
            verifyRes = ""
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
            
                var i = 0
                for signature in verifier.signatures {
                    i+=1
                    verifyRes += "Signature# \(i) \n"
		    verifyRes += " Signature Type: \(signSignatureType(value: signature.signatureType)) \n"
                    verifyRes += " Level: \(signLevel(value: signature.level)) \n"
                    verifyRes += " Subject RDN: \(signature.subjectRDN) \n"
                    verifyRes += " Timestamp: \(signature.claimedSigningTime) \n"
                    verifyRes += " Signature Validation Result: \(signValidRes(value: signature.signatureValidationResult)) \n"
                    verifyRes += " Chain Validation Result: \(signChainRes(value: signature.chainValidationResult)) \n"
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
    
    func onChainValidated(subjectRDN: String, validationResult: Int32, validationDetails: Int32) {}
    func onDecryptionInfoNeeded(cancelDecryption: inout Bool) {}
    func onError(errorCode: Int32, description: String) {}
    func onNotification(eventID: String, eventParam: String) {}
    func onRecipientFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool) {}
    
    func onSignatureFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {
        if (certFound) {
            validateSignature = true
            validateChain = true
        } else {
            showMessage = ShowMes(message: "Certificate not found")
        }
    }
    
    func onSignatureValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onTimestampFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateTimestamp: inout Bool, validateChain: inout Bool) {}
    func onTimestampValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, time: String, validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32) {}
    func onTLSCertValidate(serverHost: String, serverIP: String, accept: inout Bool) {}
    func onChainElementDownload(kind: Int32, certRDN: String, caCertRDN: String, location: String, action: inout Int32) {}
    func onChainElementNeeded(kind: Int32, certRDN: String, caCertRDN: String) {}
    func onChainValidated(index: Int32, subjectRDN: String, validationResult: Int32, validationDetails: Int32) {}
    func onChainValidationProgress(eventKind: String, certRDN: String, caCertRDN: String, action: inout Int32) {}
    func onDocumentLoaded(cancel: inout Bool) {}
    func onEncrypted(certUsed: Bool, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, needCredential: Bool, skipThis: inout Bool) {}
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
