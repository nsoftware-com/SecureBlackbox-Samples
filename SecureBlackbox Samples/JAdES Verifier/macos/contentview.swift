import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

func signLevel(value: JAdESSignatureLevels) -> String {
    switch (value) {
        case JAdESSignatureLevels.jaslGeneric: return "JWS"
        case JAdESSignatureLevels.jaslBaselineB: return "Baseline-B"
        case JAdESSignatureLevels.jaslBaselineT: return "Baseline-T"
        case JAdESSignatureLevels.jaslBaselineLT: return "Baseline-LT"
        case JAdESSignatureLevels.jaslBaselineLTA: return "Baseline-LTA"
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

struct ContentView: View, JAdESVerifierDelegate {
    var verifier = JAdESVerifier()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var dataFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var verifyRes: String = ""
    @State private var showMessage: ShowMes?
                    
    var body: some View {
        VStack(alignment: .center) {
            Text("Input file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
#if os(iOS)
            TextField("", text: $inputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
#else
            TextField("", text: $inputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openInputButton()
#endif
            }
            
            
            Text("Data file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
#if os(iOS)
            TextField("", text: $dataFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(.none)
#else
            TextField("", text: $dataFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openInputButton()
#endif
            }
            
            
            Text("Certificate file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
#if os(iOS)
            TextField("", text: $certFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
#else
            TextField("", text: $certFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openCertButton()
#endif
            }
            
            
            Text("Password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
#if os(iOS)
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
#else
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
#endif
            verifyButton()
            TextEditor(text: $verifyRes)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                .border(Color.black, width: 1)
                
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }
    
#if !os(iOS)
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
#endif
        
#if !os(iOS)
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
#endif

    @ViewBuilder
    private func verifyButton() -> some View {
        Button(action: {
            //verifier.runtimeLicense = ""
            verifier.delegate = self
            showMessage = nil
            verifyRes = ""
#if os(iOS)
            verifier.inputFile = documentsPath + inputFile
            verifier.dataFile = documentsPath + dataFile
            let certPath = documentsPath + certFile
#else
            verifier.inputFile = inputFile
            verifier.dataFile = dataFile
            let certPath = certFile
#endif
            if (certFile != "") {
                do {
                    try certmanager.importFromFile(path: certPath, password: certPass)
                    verifier.knownCertificates.append(certmanager.certificate)
                } catch {
                    showMessage = ShowMes(message: "Failed to load certificate: \(error)")
                    return
                }
            }
            
            do {
                try verifier.verify()
            
                var i = 0
                for signature in verifier.signatures {
                    i+=1
                    verifyRes += "Signature# \(i) \n"
                    verifyRes += " Level: \(signLevel(value: signature.level)) \n"
                    verifyRes += " Subject RDN: \(signature.subjectRDN) \n"
                    verifyRes += " Claimed Signing Time: \(signature.claimedSigningTime) \n"
                    verifyRes += " Validated Signing Time: \(signature.validatedSigningTime) \n"
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
    
    func onSignatureFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {
        if (certFound) {
            validateSignature = true
            validateChain = true
        } else {
            showMessage = ShowMes(message: "Certificate not found")
        }
    }
   
    func onChainElementDownload(kind: Int32, certRDN: String, caCertRDN: String, location: String, action: inout Int32) {}
    func onChainElementNeeded(kind: Int32, certRDN: String, caCertRDN: String) {}
    func onChainElementStore(kind: Int32, body: Data, uri: inout String) {}
    func onChainValidated(index: Int32, subjectRDN: String, validationResult: Int32, validationDetails: Int32) {}
    func onChainValidationProgress(eventKind: String, certRDN: String, caCertRDN: String, action: inout Int32) {}
    func onError(errorCode: Int32, description: String) {}
    func onHTTPHeaderFieldNeeded(fieldName: String, fieldValues: inout String) {}
    func onLoaded(cancel: inout Bool) {}
    func onNotification(eventID: String, eventParam: String) {}
    func onObjectNeeded(uri: String, contentType: String, base64: inout Bool) {}
    func onObjectValidate(uri: String, contentType: String, hashAlgorithm: String, hash: Data, base64: Bool, valid: inout Bool) {}
    func onSignatureFound(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {}
    func onSignatureValidated(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onTimestampFound(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateTimestamp: inout Bool, validateChain: inout Bool) {}
    func onTimestampValidated(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, time: String, validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32) {}
    func onTLSCertNeeded(host: String, caNames: String) {}
    func onTLSCertValidate(serverHost: String, serverIP: String, accept: inout Bool) {}
    func onTLSEstablished(host: String, version: String, ciphersuite: String, connectionId: Data, abort: inout Bool) {}
    func onTLSHandshake(host: String, abort: inout Bool) {}
    func onTLSShutdown(host: String) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
