import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var crypto = SymmetricCrypto()
    var keymanager = CryptoKeyManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var compact = false
    @State private var inputStr: String = ""
    @State private var password: String = ""
    @State private var encryptedStr: String = ""
    @State private var decryptedStr: String = ""
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Plaintext string:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $inputStr)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            Text("Encryption password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $password)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            Toggle(isOn: $compact) {
                Text("Compact")
            }
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                        
            Text("Encrypted token:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $encryptedStr)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            Text("Decrypted string:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $decryptedStr)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            HStack(alignment: .firstTextBaseline) {
                encryptButton()
                decryptButton()
            }
            
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func encryptButton() -> some View {
        Button(action: {
            //crypto.runtimeLicense = ""
            showMessage = nil
            encryptedStr = ""
            crypto.encryptionAlgorithm = "AES256"
            crypto.inputEncoding = SymmetriccryptoInputEncodings.cetBinary
            if (compact) {
                crypto.outputEncoding = SymmetriccryptoOutputEncodings.cetCompact
            } else {
                crypto.outputEncoding = SymmetriccryptoOutputEncodings.cetJSON
            }
            
            do {
                // key from password
                try keymanager.deriveKey(keyBits: 256, password: password, salt: "")
                let IV = Data.init(count: 16)
                keymanager.key.iv = IV
                crypto.key = keymanager.key
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
            
            do {
                let inBuf = inputStr.data(using: .utf8)
                let outBuf = try crypto.encrypt(buffer: inBuf!)
                encryptedStr = String(decoding: outBuf, as: UTF8.self)
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Encrypt").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func decryptButton() -> some View {
        Button(action: {
            //crypto.runtimeLicense = ""
            showMessage = nil
            decryptedStr = ""
            
            do {
                crypto.encryptionAlgorithm = "AES256"
                if (compact) {
                    crypto.inputEncoding = SymmetriccryptoInputEncodings.cetCompact
                } else {
                    crypto.inputEncoding = SymmetriccryptoInputEncodings.cetJSON
                }
                crypto.outputEncoding = SymmetriccryptoOutputEncodings.cetBinary
                
                do {
                    // key from password
                    try keymanager.deriveKey(keyBits: 256, password: password, salt: "")
                    let IV = Data.init(count: 16)
                    keymanager.key.iv = IV
                    crypto.key = keymanager.key
                } catch {
                    showMessage = ShowMes(message: "Error \(error)")
                    return
                }
                
                let inBuf = encryptedStr.data(using: .utf8)
                let outBuf = try crypto.decrypt(buffer: inBuf!)
                decryptedStr = String(decoding: outBuf, as: UTF8.self)
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Decrypt").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
