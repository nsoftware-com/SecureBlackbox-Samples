import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var authenticator = Authenticator()
    var usermanager = UserManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var isStart = true
    @State private var usersFile: String = ""
    @State private var usersPass: String = ""
    @State private var userId: String = ""
    @State private var authMethod: String = ""
    @State private var authToken: String = ""
    @State private var showMessage: ShowMes?
    
    func chechAuthResult(value: Int32) {
        if (value != 0) {
            isStart = true;
        }
        
        switch (value) {
            case 0:
                authMethod = authenticator.authInfo.authMethod
                authToken = ""
                isStart = false
                break
            case 1:
                showMessage = ShowMes(message: "Authentication succeeded", title: "Info")
                break
            case 2:
                showMessage = ShowMes(message: "Authentication failed", title: "Info")
                break
            default:
                showMessage = ShowMes(message: "Canceled by user", title: "Info")
                break
        }
    }
    
    var body: some View {
        VStack(alignment: .center) {
            if (isStart) {
                Text("Users file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $usersFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(.none)
                }
                
                
                Text("Password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $usersPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(.none)
                Text("User id:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $userId)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                startButton()
            }
            else
            {
                Text("Auth method:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $authMethod)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(.none)
                Text("Auth token:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $authToken)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(.none)
                HStack(alignment: .firstTextBaseline) {
                    cancelButton()
                    continueButton()
                }
                
            }
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func startButton() -> some View {
        Button(action: {
            //authenticator.runtimeLicense = ""
            showMessage = nil
            do {
                try usermanager.load(filename: documentsPath + usersFile, password: usersPass)
                while authenticator.users.count > 0 {
                    authenticator.users.remove(at: 0)
                }
                for user in usermanager.users {
                    authenticator.users.append(user)
                }
                
                var userFind = false
                for user in authenticator.users {
                    if (user.username == userId) {
                        userFind = true
                        break
                    }
                }
                
                if (userFind) {
                    let res = try authenticator.startAuth(userID: userId)
                    chechAuthResult(value: res)
                } else {
                    showMessage = ShowMes(message: "Authentication failed. User not found", title: "Warning")
                    return
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Start").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func cancelButton() -> some View {
        Button(action: {
            chechAuthResult(value: 3)
        }, label: {
            Text("Cancel").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func continueButton() -> some View {
        Button(action: {
            showMessage = nil
            do {
                let res = try authenticator.continueAuth(state: authenticator.authInfo.state, authToken: authToken)
                chechAuthResult(value: res)
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Continue").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
