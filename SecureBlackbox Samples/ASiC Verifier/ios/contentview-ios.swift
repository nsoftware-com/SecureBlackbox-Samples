import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

extension Data {
    struct HexEncodingOptions: OptionSet {
        let rawValue: Int
        static let upperCase = HexEncodingOptions(rawValue: 1 << 0)
    }

    func hexEncodedString(options: HexEncodingOptions = []) -> String {
        let format = options.contains(.upperCase) ? "%02hhX" : "%02hhx"
        return self.map { String(format: format, $0) }.joined()
    }
}

struct Contentview: View, ASiCVerifierDelegate {
    var verifier = ASiCVerifier()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var outputRes: String = ""
    @State private var showMessage: ShowMes?
        
    func levelStr(value: AsicverifierLevels) -> String {
        switch (value) {
            case AsicverifierLevels.aslGeneric: return "Generic"
            case AsicverifierLevels.aslBaselineB: return "Baseline B"
            case AsicverifierLevels.aslBaselineT: return "Baseline T"
            case AsicverifierLevels.aslBaselineLT: return "Baseline LT"
            case AsicverifierLevels.aslBaselineLTA: return "Baseline LTA"
            case AsicverifierLevels.aslBES: return "BES"
            case AsicverifierLevels.aslEPES: return "EPES"
            case AsicverifierLevels.aslT: return "T"
            case AsicverifierLevels.aslC: return "C"
            case AsicverifierLevels.aslX: return "X"
            case AsicverifierLevels.aslXType1: return "XType1"
            case AsicverifierLevels.aslXType2: return "XType"
            case AsicverifierLevels.aslXL: return "XL"
            case AsicverifierLevels.aslXLType1: return "XLType1"
            case AsicverifierLevels.aslXLType2: return "XLType2"
            case AsicverifierLevels.aslExtendedBES: return "Extended BES"
            case AsicverifierLevels.aslExtendedEPES: return "Extended EPES"
            case AsicverifierLevels.aslExtendedT: return "Extended T"
            case AsicverifierLevels.aslExtendedC: return "Extended C"
            case AsicverifierLevels.aslExtendedX: return "Extended X"
            case AsicverifierLevels.aslExtendedXType1: return "Extended XType1"
            case AsicverifierLevels.aslExtendedXType2: return "Extended XType"
            case AsicverifierLevels.aslExtendedXLong: return "Extended XLong"
            case AsicverifierLevels.aslExtendedXL: return "Extended XL"
            case AsicverifierLevels.aslExtendedXLType1: return "Extended XLType1"
            case AsicverifierLevels.aslExtendedXLType2: return "Extended XLType2"
            case AsicverifierLevels.aslExtendedA: return "Extended A"
            case AsicverifierLevels.aslA: return "A"
            default: return "Unknown"
        }
    }

    func signType(value: ASiCSignatureTypes) -> String {
        switch (value) {
            case ASiCSignatureTypes.castCAdES: return "CAdES"
            case ASiCSignatureTypes.castXAdES: return "XAdES"
            case ASiCSignatureTypes.castTimestamp: return "Timestamp"
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
            Text("Output:")
            TextEditor(text: $outputRes)
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
            outputRes = ""
            verifier.extractionMode = AsicverifierExtractionModes.aemAll
            verifier.inputFile = documentsPath + inputFile
            verifier.outputPath = documentsPath
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
                    i += 1
                    outputRes += "Signature# \(i) \n"
                    outputRes += "    Level: \(levelStr(value: verifier.level)) \n"
                    outputRes += "    Signature type: \(signType(value: signature.signatureType)) \n"
                    outputRes += "    Issuer RDN: \( signature.issuerRDN) \n"
                    outputRes += "    Serial number: \(signature.serialNumber.hexEncodedString()) \n"
                    outputRes += "    Subject KeyID: \(signature.subjectKeyID.hexEncodedString()) \n"
                    outputRes += "    Signed Files: \(signature.signedFiles) \n"
                    outputRes += "    Signature Validation Result: \(signValidRes(value: signature.signatureValidationResult)) \n"
                    outputRes += "    Chain Validation Result: \(signChainRes(value:  signature.chainValidationResult)) \n"
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
    func onError(errorCode: Int32, description: String) {}
    func onFileExtractionStart(fileName: String, fileSize: Int64, modDate: String, destFile: inout String) {}
    func onNotification(eventID: String, eventParam: String) {}
    func onRetrieveCertificate(uri: String) {}
    func onRetrieveCRL(uri: String) {}
    func onRetrieveOCSPResponse(uri: String) {}
    
    func onSignatureFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {
        if (certFound) {
            validateSignature = true
            validateChain = true
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
    func onTLSCertValidate(serverHostname: String, serverIP: String, accept: inout Bool) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        Contentview()
    }
}
