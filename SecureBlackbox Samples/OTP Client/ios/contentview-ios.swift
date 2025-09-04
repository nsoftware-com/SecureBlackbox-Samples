import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var client = OTPClient()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var keySecret: String = ""
    @State private var passLength: String = "10"
    @State private var passAlgorithm = 0
    @State private var counter: String = "0"
    @State private var passRes: String = ""
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Secret key:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $keySecret)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(.none)
            Text("Password length:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $passLength)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .keyboardType(.numberPad)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            Text("Algorithm:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Picker(selection: $passAlgorithm, label: Text("")) {
                Text("HMAC")
                    .tag(0)
                Text("Time")
                    .tag(1)
            }
            .pickerStyle(SegmentedPickerStyle())
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)

            Text("Counter:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $counter)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .keyboardType(.numberPad)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            generateButton()
            TextField("", text: $passRes)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func generateButton() -> some View {
        Button(action: {
            //client.runtimeLicense = ""
            showMessage = nil
            passRes = ""
            
            do {
                let secret = keySecret.data(using: .utf8)
                client.keySecret = secret!
                client.passwordLength = Int32(passLength) ?? 10
                
                if (passAlgorithm == 1) {
                    passRes = try client.generateTOTPPassword(timeInterval: Int32(counter) ?? 0, hashAlgorithm: "SHA256")
                } else {
                    passRes = try client.generateHOTPPassword(counter: Int32(counter) ?? 0)
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Generate password").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
