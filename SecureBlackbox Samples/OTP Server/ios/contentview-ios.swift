import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var server = OTPServer()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var keySecret: String = ""
    @State private var passLength: String = "10"
    @State private var passAlgorithm = 0
    @State private var counter: String = "0"
    @State private var delta: String = "0"
    @State private var password: String = ""
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
            Group
            {
                Text("Delta:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $delta)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .keyboardType(.numberPad)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                Text("Password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $password)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            verifyButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func verifyButton() -> some View {
        Button(action: {
            //server.runtimeLicense = ""
            showMessage = nil
            
            do {
                var res = false
                try _ = server.config(configurationString: "Delta=" + delta)
                let secret = keySecret.data(using: .utf8)
                
                if (passAlgorithm == 1) {
                    res = try server.isTOTPPasswordValid(keySecret: secret!, passwordLength: Int32(passLength) ?? 10, timeInterval: Int32(counter) ?? 0, hashAlgorithm: "SHA256", password: password)
                } else {
                    res = try server.isHOTPPasswordValid(keySecret: secret!, passwordLength: Int32(passLength) ?? 10, counter: Int32(counter) ?? 0, password: password)
                }
                
                if (res) {
                    showMessage = ShowMes(message: "Password is valid", title: "Info")
                } else {
                    showMessage = ShowMes(message: "Password is not valid", title: "Info")
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Is password valid").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
