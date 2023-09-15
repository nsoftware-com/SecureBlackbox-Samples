import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, XAdESVerifierDelegate {

    var verifier = XAdESVerifier()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var detached = false
    @State private var dataFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var references: String = ""
    @State private var showMessage: ShowMes?
          
    func getVersion(value: XAdESVersions) -> String {
        switch (value) {
            case XAdESVersions.xav111: return "1.1.1"
            case XAdESVersions.xav122: return "1.2.2"
            case XAdESVersions.xav132: return "1.3.2"
            case XAdESVersions.xav141: return "1.4.1"
            default: return "Unknown"
        }
    }

    func getForm(value: XAdESForms) -> String {
        switch (value) {
            case XAdESForms.xafBasic: return "Basic"
            case XAdESForms.xafBES: return "BES"
            case XAdESForms.xafEPES: return "EPES"
            case XAdESForms.xafT: return "T"
            case XAdESForms.xafC: return "C"
            case XAdESForms.xafX: return "X"
            case XAdESForms.xafXL: return "XL"
            case XAdESForms.xafA: return "A"
            case XAdESForms.xafExtendedBES: return "Extended BES"
            case XAdESForms.xafExtendedEPES: return "Extended EPES"
            case XAdESForms.xafExtendedT: return "Extended T"
            case XAdESForms.xafExtendedC: return "Extended C"
            case XAdESForms.xafExtendedX: return "Extended X"
            case XAdESForms.xafExtendedXLong: return "Extended XLong"
            case XAdESForms.xafExtendedXL: return "Extended XL"
            case XAdESForms.xafExtendedA: return "Extended A"
            default: return "Unknown"
        }
    }
    
    var body: some View {
        VStack(alignment: .center) {
            Text("Input file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $inputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openInputButton()
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
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openDataButton()
                }
                
            }
            Group {
                Text("Certificate file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $certFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openCertButton()
                }
                
                
                Text("Password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $certPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
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
    private func openDataButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title = "Choose an data file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    dataFile = result!.path
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
    private func verifyButton() -> some View {
        Button(action: {
            //verifier.runtimeLicense = ""
            verifier.delegate = self
            showMessage = nil
            references = ""
            verifier.inputFile = inputFile
            let certPath = certFile
            if (certFile != "") {
                do {
                    try certmanager.importFromFile(path: certPath, password: certPass)
                    verifier.knownCertificates.append(certmanager.certificate);
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
                    verifier.dataFile = dataFile
                    verifier.dataType = XadesverifierDataTypes.cxdtBinary
                    verifier.dataURI = (dataFile as NSString).lastPathComponent
                    
                    try verifier.verifyDetached()
                } else {
                    try verifier.verify()
                }
                
                var i = 0
                for signature in verifier.signatures {
                    i+=1
                    references += "\nSignature# \(i) \n"
                    
                    switch (signature.signatureValidationResult) {
                        case XMLSignatureValidities.xsvValid:
                            if (signature.xAdES) {
                                references += "\nXAdES version: \(getVersion(value: signature.xAdESVersion)) \n"
                                references += "\nXAdES form: \(getForm(value: signature.xAdESForm)) \n"
                            }
                            switch (signature.chainValidationResult) {
                                case ChainValidities.cvtInvalid:
                                    showMessage = ShowMes(message: "Signing certificate is not valid.", title: "Info")
                                    break
                                case ChainValidities.cvtCantBeEstablished:
                                    showMessage = ShowMes(message: "Signing certificate chain could not be validated completely.", title: "Info")
                                    break
                                case ChainValidities.cvtValidButUntrusted:
                                    showMessage = ShowMes(message: "The selected signature is signed by self-signed certificate which was not previously trusted.", title: "Info")
                                    break
                                default:
                                    showMessage = ShowMes(message: "Signature validated successfully.", title: "Info")
                                    break
                            }
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
    
    func onTimestampValidated(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, time: String, validationResult: Int32, chainValidationResult: Int32, chainValidationDetails: Int32) {
        switch (validationResult) {
            case 0: //svtValid
                references += "Timestamp signature is valid \n"
                break
            case 2: //svtCorrupted
                references += "Timestamp signature is corrupted \n"
                break
            case 4: //svtFailure
                references += "Timestamp signature is failure \n"
                break
            case 3: //svtSignerNotFound
                references += "Timestamp signature is not found \n"
                break
            default:
                references += "Timestamp signature is unknown \n"
                break
        }
    }
        
    func onTLSCertValidate(serverHost: String, serverIP: String, accept: inout Bool) {}
    func onChainElementDownload(kind: Int32, certRDN: String, caCertRDN: String, location: String, action: inout Int32) {}
    func onChainElementNeeded(kind: Int32, certRDN: String, caCertRDN: String) {}
    func onChainElementStore(kind: Int32, body: Data, uri: inout String) {}
    func onChainValidated(index: Int32, subjectRDN: String, validationResult: Int32, validationDetails: Int32) {}
    func onChainValidationProgress(eventKind: String, certRDN: String, caCertRDN: String, action: inout Int32) {}
    func onDocumentLoaded(cancel: inout Bool) {}
    func onReferenceValidated(referenceIndex: Int32, id: String, uri: String, refType: String, digestValid: Bool) {}
    func onResolveReference(referenceIndex: Int32, uri: String) {}
    func onSignatureFound(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {}
    func onSignatureValidated(sigIndex: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
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
