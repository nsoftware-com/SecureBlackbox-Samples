import SwiftUI
import SecureBlackbox

extension String {
    var hexadecimal: Data? {
        var data = Data(capacity: count / 2)
        let regex = try! NSRegularExpression(pattern: "[0-9a-f]{1,2}", options: .caseInsensitive)
        regex.enumerateMatches(in: self, range: NSRange(startIndex..., in: self)) { match, _, _ in
            let byteString = (self as NSString).substring(with: match!.range)
            let num = UInt8(byteString, radix: 16)!
            data.append(num)
        }
        
        guard data.count > 0 else { return nil }
        return data
    }
}

extension Data {
    var hexadecimal: String {
        return map { String(format: "%02x", $0) }
            .joined()
    }
}

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, PDFSignerDelegate {
    var signer = PDFSigner()
    var certmanager = CertificateManager()
    var keymanager = CryptoKeyManager()
    var crypto = PublicKeyCrypto()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var keyFile: String = ""
    @State private var signVisible = true
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Input file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $inputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openInputButton()
            }
            
            
            Text("Output file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $outputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            saveOutputButton()
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
                    
                SecureField("", text: $certPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Text("Key file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $keyFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openKeyButton()
                }
                
            }
            Toggle(isOn: $signVisible) {
                Text("Visible signature")
            }
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            
            signButton()
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
    private func saveOutputButton() -> some View {
        Button(action: {
            let dialog = NSSavePanel()
            dialog.title = "Choose an output file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    outputFile = result!.path
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
    private func openKeyButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title                   = "Choose an certificate file"
            dialog.showsResizeIndicator    = true
            dialog.showsHiddenFiles        = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    keyFile = result!.path
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
            //signer.runtimeLicense = ""
            signer.delegate = self
            showMessage = nil
            signer.inputFile = inputFile
            signer.outputFile = outputFile
            let certPath = certFile
            signer.widget.invisible = !signVisible

            do {
                try certmanager.importFromFile(path: certPath, password: certPass)
                signer.signingCertificate = certmanager.certificate
                
                if (certmanager.certificate.keyAlgorithm.range(of: "id-dsa") != nil) {
                    signer.newSignature.hashAlgorithm = "SHA1"
                }
            
                signer.newSignature.authorName = "test demo author"
                signer.newSignature.reason = "test demo reason"
                signer.ignoreChainValidationErrors = true
                signer.externalCrypto.mode = ExternalCryptoModes.ecmGeneric
                
                try signer.signExternal()
                showMessage = ShowMes(message: "The file successfully signed", title: "Info")
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Sign").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    func onDecryptionInfoNeeded(cancelDecryption: inout Bool) {}
    func onError(errorCode: Int32, description: String) {}
    func onExternalDecrypt(operationId: String, algorithm: String, pars: String, encryptedData: String, data: inout String) {}
        
    func onExternalSign(operationId: String, hashAlgorithm: String, pars: String, data: String, signedData: inout String) {
        do {
            try keymanager.importFromFile(filename: keyFile, format: 3 , keyAlgorithm: "", scheme: "", schemeParams: pars, keyType: 0)
            crypto.key = keymanager.key
            crypto.hashAlgorithm = hashAlgorithm
            crypto.inputIsHash = true
            crypto.schemeParams = pars
            let inBuf = data.hexadecimal
            let outBuf = try crypto.sign(buffer: inBuf!, detached: true)
            signedData = outBuf.hexadecimal
        } catch {
            showMessage = ShowMes(message: "Error \(error)")
        }
    }
        
    func onNotification(eventID: String, eventParam: String) {}
    func onRecipientFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool) {}
    func onTLSCertValidate(serverHost: String, serverIP: String, accept: inout Bool) {}
    func onChainElementDownload(kind: Int32, certRDN: String, caCertRDN: String, location: String, action: inout Int32) {}
    func onChainElementNeeded(kind: Int32, certRDN: String, caCertRDN: String) {}
    func onChainValidated(index: Int32, subjectRDN: String, validationResult: Int32, validationDetails: Int32) {}
    func onChainValidationProgress(eventKind: String, certRDN: String, caCertRDN: String, action: inout Int32) {}
    func onDocumentLoaded(cancel: inout Bool) {}
    func onEncrypted(certUsed: Bool, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, needCredential: Bool, skipThis: inout Bool) {}
    func onPreRenderWidget(addAnother: inout Bool, resetAnother: inout Bool) {}
    func onSignatureFound(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateSignature: inout Bool, validateChain: inout Bool) {}
    func onSignatureValidated(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, validationResult: Int32) {}
    func onTimestampFound(index: Int32, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool, validateTimestamp: inout Bool, validateChain: inout Bool) {}
    func onTimestampRequest(tsa: String, timestampRequest: String, timestampResponse: inout String, suppressDefault: inout Bool) {}
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
