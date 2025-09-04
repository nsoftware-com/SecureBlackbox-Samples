import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, XMLDecryptorDelegate {
    var decryptor = XMLDecryptor()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var decPass: String = ""
    @State private var docInfo: String = ""
    @State private var firstPassRequest = true
    @State private var showMessage: ShowMes?
      
    func getKey(algorithm: String) -> Data {
        var len = 0;
        if (algorithm.caseInsensitiveCompare("AES128") == ComparisonResult.orderedSame) {
            len = 16
        } else if (algorithm.caseInsensitiveCompare("AES192") == ComparisonResult.orderedSame) {
            len = 24
        } else if (algorithm.caseInsensitiveCompare("AES256") == ComparisonResult.orderedSame) {
            len = 32
        } else if (algorithm.caseInsensitiveCompare("Camellia128") == ComparisonResult.orderedSame) {
            len = 16
        } else if (algorithm.caseInsensitiveCompare("Camellia192") == ComparisonResult.orderedSame) {
            len = 24
        } else if (algorithm.caseInsensitiveCompare("Camellia256") == ComparisonResult.orderedSame) {
            len = 32
        } else if (algorithm.caseInsensitiveCompare("DES") == ComparisonResult.orderedSame) {
            len = 8
        } else if (algorithm.caseInsensitiveCompare("3DES") == ComparisonResult.orderedSame) {
            len = 24
        } else if (algorithm.caseInsensitiveCompare("RC4") == ComparisonResult.orderedSame) {
            len = 16
        } else if (algorithm.caseInsensitiveCompare("SEED") == ComparisonResult.orderedSame) {
            len = 16
        }
        
        var res = decPass;
        while (res.count < len) {
            res += "/\(decPass)"
        }
        return res.substring(to: res.index(res.startIndex, offsetBy: len)).data(using: .utf8)!
    }
    
    var body: some View {
        VStack(alignment: .center) {
            Text("Input file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $inputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(.none)
            }
            
            
            Text("Output file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $outputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            
            
            Group
            {
                Text("Provide password for decryption:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $decPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                Text("Provide certificate file for decryption:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $certFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                
                
                Text("Certificate password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $certPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            decryptButton()
            TextEditor(text: $docInfo)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                .border(Color.black, width: 1)
                
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func decryptButton() -> some View {
        Button(action: {
            //decryptor.runtimeLicense = ""
            decryptor.delegate = self
            showMessage = nil
            firstPassRequest = true
            docInfo = ""
            do {
#if os(iOS)
                decryptor.inputFile = documentsPath + inputFile
                decryptor.outputFile = documentsPath + outputFile
                try decryptor.decrypt()
                showMessage = ShowMes(message: "The file successfully decrypted", title: "Info")
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Decrypt").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    func onDecryptionInfoNeeded(cancelDecryption: inout Bool) {
        if (!firstPassRequest) {
            return
        }
        
        firstPassRequest = false
        docInfo += "Encryption method: \(decryptor.encryptionMethod)"
        if (decryptor.encryptedDataType == XmldecryptorEncryptedDataTypes.cxedtExternal) {
            docInfo += "\nEncrypted data type: external"
        } else if (decryptor.encryptedDataType == XmldecryptorEncryptedDataTypes.cxedtContent) {
            docInfo += "\nEncrypted data type: content"
        } else {
            docInfo += "\nEncrypted data type: element"
        }
                 
        if (decryptor.encryptKey) {
            docInfo += "\nEncryptKey: true"
            if (decryptor.keyEncryptionType == XmldecryptorKeyEncryptionTypes.cxetKeyTransport) {
                docInfo += "\nKey encryption type: transport"
                
                if (decryptor.keyTransportMethod == XmldecryptorKeyTransportMethods.cxktRSA15) {
                    docInfo += "\nKey transport method: RSA v1.5"
                } else {
                    docInfo += "\nKey transport method: RSA-OAEP"
                }
                
                if (certFile != "") {
                    do {
                        try certmanager.importFromFile(path: documentsPath + certFile, password: certPass)
                        decryptor.keyDecryptionCertificate = certmanager.certificate
                        cancelDecryption = false
                    } catch {
                        showMessage = ShowMes(message: "Error \(error)")
                        cancelDecryption = true
                    }
                } else {
                    showMessage = ShowMes(message: "Certificate for decryption not set")
                    cancelDecryption = true
                }
            } else {
                docInfo += "\nKey encryption type: wrap"
                docInfo += "\nKey wrap method: \(decryptor.keyWrapMethod)"
                decryptor.keyDecryptionKey = getKey(algorithm: decryptor.keyWrapMethod)
                cancelDecryption = false
            }
        } else {
            docInfo += "\nEncryptKey: false"
            decryptor.decryptionKey = getKey(algorithm: decryptor.encryptionMethod)
            cancelDecryption = false
        }
    }
    
    func onError(errorCode: Int32, description: String) {}
    func onExternalDecrypt(operationId: String, algorithm: String, pars: String, encryptedData: String, data: inout String) {}
    func onNotification(eventID: String, eventParam: String) {}
    func onSaveExternalData(externalData: Data) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
