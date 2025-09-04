import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var encryptor = MessageEncryptor()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var showMessage: ShowMes?
        
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
            encryptor.encryptionAlgorithm = "AES256"
            encryptor.inputFile = documentsPath + inputFile
            encryptor.outputFile = documentsPath + outputFile
            let certPath = documentsPath + certFile
            do {
                try certmanager.importFromFile(path: certPath, password: certPass)
                encryptor.encryptionCertificate = certmanager.certificate

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
