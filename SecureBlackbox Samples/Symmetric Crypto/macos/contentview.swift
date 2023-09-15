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

struct ContentView: View {
    var crypto = SymmetricCrypto()
    var keymanager = CryptoKeyManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var encodingType = 0
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
                
            Text("Encryption password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $password)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Group
            {
                Text("Encoding type:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Picker(selection: $encodingType, label: Text("")) {
                    Text("Binary")
                        .tag(0)
                    Text("Base64")
                        .tag(1)
                    Text("Compact")
                        .tag(2)
                    Text("JSON")
                        .tag(3)
                }
                .pickerStyle(SegmentedPickerStyle())
                
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                        
                Text("Encrypted token:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $encryptedStr)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Text("Decrypted string:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $decryptedStr)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
            }
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
            switch (encodingType) {
                case 1:
                    crypto.outputEncoding = SymmetriccryptoOutputEncodings.cetBase64
                    break
                case 2:
                    crypto.outputEncoding = SymmetriccryptoOutputEncodings.cetCompact
                    break
                case 3:
                    crypto.outputEncoding = SymmetriccryptoOutputEncodings.cetJSON
                    break
                default:
                    crypto.outputEncoding = SymmetriccryptoOutputEncodings.cetBinary
            }
            
            do {
                // key from password
                if ((crypto.outputEncoding == SymmetriccryptoOutputEncodings.cetBinary) || (crypto.outputEncoding == SymmetriccryptoOutputEncodings.cetBase64)) {
                    try keymanager.deriveKey(keyBits: 256, password: password, salt: "")
                } else {
                    try keymanager.deriveKey(keyBits: 512, password: password, salt: "")
                }
                            
                let IV = Data.init(count: 16)
                keymanager.key.iv = IV
                crypto.key = keymanager.key
                let inBuf = inputStr.data(using: .utf8)
                let outBuf = try crypto.encrypt(buffer: inBuf!)
                if (crypto.outputEncoding == SymmetriccryptoOutputEncodings.cetBinary) {
                    encryptedStr = outBuf.hexEncodedString()
                } else {
                    encryptedStr = String(decoding: outBuf, as: UTF8.self)
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)");
                return;
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
            crypto.encryptionAlgorithm = "AES256"
                
            switch (encodingType) {
                case 1:
                    crypto.inputEncoding = SymmetriccryptoInputEncodings.cetBase64
                    break
                case 2:
                    crypto.inputEncoding = SymmetriccryptoInputEncodings.cetCompact
                    break
                case 3:
                    crypto.inputEncoding = SymmetriccryptoInputEncodings.cetJSON
                    break
                default:
                    crypto.inputEncoding = SymmetriccryptoInputEncodings.cetBinary
            }
            crypto.outputEncoding = SymmetriccryptoOutputEncodings.cetBinary
                
            do {
                // key from password
                if ((crypto.inputEncoding == SymmetriccryptoInputEncodings.cetBinary) || (crypto.inputEncoding == SymmetriccryptoInputEncodings.cetBase64)) {
                    try keymanager.deriveKey(keyBits: 256, password: password, salt: "")
                } else {
                    try keymanager.deriveKey(keyBits: 512, password: password, salt: "")
                }
                                
                let IV = Data.init(count: 16)
                keymanager.key.iv = IV
                crypto.key = keymanager.key
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
            
            do  {
                let inBuf: Data?
                if (crypto.inputEncoding == SymmetriccryptoInputEncodings.cetBinary) {
                    inBuf = encryptedStr.hexadecimal
                } else {
                    inBuf = encryptedStr.data(using: .utf8)
                }
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
