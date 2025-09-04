import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var encryptor = XMLEncryptor()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var encKey = false
    @State private var encKeyType = 0
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var encPass: String = ""
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
        
        var res = encPass
        while (res.count < len) {
            res += "/\(encPass)"
        }
        return res.substring(to: res.index(res.startIndex, offsetBy: len)).data(using: .utf8)!;
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
            
            
            Text("Output file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $outputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            
            
            Toggle(isOn: $encKey) {
                Text("Encrypt Key")
            }
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)

            Text("Key encryption type:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .disabled(encKey == false)
            Picker(selection: $encKeyType, label: Text("")) {
                Text("Key Transport")
                    .tag(0)
                Text("Key Wrap")
                    .tag(1)
            }
            .pickerStyle(SegmentedPickerStyle())
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            .disabled(encKey == false)
            
            if (encKey && encKeyType == 0) {
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
            } else {
                Text("Encryption password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $encPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            encryptButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func encryptButton() -> some View {
        Button(action: {
            //encryptor.runtimeLicense = ""
            showMessage = nil
            encryptor.inputFile = documentsPath + inputFile
            encryptor.outputFile = documentsPath + outputFile
            let certPath = documentsPath + certFile
            encryptor.encryptKey = encKey
            encryptor.encryptedDataType = XmlencryptorEncryptedDataTypes.cxedtElement
            encryptor.encryptionMethod = "AES256"
            
            if (encKey) {
                if (encKeyType == 0) {
                    encryptor.keyEncryptionType = XmlencryptorKeyEncryptionTypes.cxetKeyTransport
                    encryptor.keyTransportMethod = XmlencryptorKeyTransportMethods.cxktRSA15
                    
                    do {
                        try certmanager.importFromFile(path: certPath, password: certPass)
                        encryptor.keyEncryptionCertificate = certmanager.certificate
                    } catch {
                        showMessage = ShowMes(message: "Error \(error)")
                        return
                    }
                } else {
                    encryptor.keyEncryptionType = XmlencryptorKeyEncryptionTypes.cxetKeyWrap
                    encryptor.keyWrapMethod = "3DES"
                    encryptor.keyEncryptionKey = getKey(algorithm: encryptor.keyWrapMethod)
                }
            } else {
                encryptor.encryptionKey = getKey(algorithm: encryptor.encryptionMethod)
            }
            
            do {
                try encryptor.encrypt()
                showMessage = ShowMes(message: "The file successfully encrypted", title: "Info")
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Encrypt").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
