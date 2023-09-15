import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var hashFunc = HashFunction()
    var keymanager = CryptoKeyManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var encodingType = 0
    @State private var inputStr: String = ""
    @State private var password: String = ""
    @State private var outputStr: String = ""
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Encoding:")
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
            
            Text("Input string:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $inputStr)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            Text("Password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $password)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            hashButton()
            Text("Output hash:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $outputStr)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func hashButton() -> some View {
        Button(action: {
            //hashFunc.runtimeLicense = ""
            showMessage = nil
            outputStr = ""
            switch (encodingType) {
                case 1:
                    hashFunc.outputEncoding = HashfunctionOutputEncodings.cetBase64
                    break
                case 2:
                    hashFunc.outputEncoding = HashfunctionOutputEncodings.cetCompact
                    break
                case 3:
                    hashFunc.outputEncoding = HashfunctionOutputEncodings.cetJSON
                    break
                default:
                    hashFunc.outputEncoding = HashfunctionOutputEncodings.cetBinary
            }
            
            hashFunc.algorithm = "SHA256"
            if (password != "") {
                do {
                    try keymanager.deriveKey(keyBits: 256, password: password, salt: "")
                    hashFunc.key = keymanager.key
                } catch {
                    showMessage = ShowMes(message: "Error \(error)")
                    return
                }
            }
            
            do {
                let inBuf = inputStr.data(using: .utf8)
                let outBuf = try hashFunc.hash(buffer: inBuf!)
                outputStr = String(decoding: outBuf, as: UTF8.self)
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Hash").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
