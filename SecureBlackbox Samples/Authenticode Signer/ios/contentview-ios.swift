import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var signer = AuthenticodeSigner()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var statementType = 0
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var timestampType = 0
    @State private var timestampServer: String = ""
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Group {
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
                
                    
                Text("Statement type:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Picker(selection: $statementType, label: Text("")) {
                    Text("Individual")
                        .tag(0)
                    Text("Commercial")
                        .tag(1)
                }
                .pickerStyle(SegmentedPickerStyle())
                
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
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
            Text("Timestamp server:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Picker(selection: $timestampType, label: Text("")) {
                Text("No")
                    .tag(0)
                Text("Trusted")
                    .tag(1)
                Text("Legacy")
                    .tag(2)
            }
            .pickerStyle(SegmentedPickerStyle())
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            
            TextField("", text: $timestampServer)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            signButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }
    
    @ViewBuilder
    private func signButton() -> some View {
        Button(action: {
            //signer.runtimeLicense = ""
            showMessage = nil
            signer.inputFile = documentsPath + inputFile
            signer.outputFile = documentsPath + outputFile
            let certPath = documentsPath + certFile

            do {
                try certmanager.importFromFile(path: certPath, password: certPass)
                signer.signingCertificate = certmanager.certificate
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
                                                        
            if (statementType == 1) {
                signer.statementType = AuthenticodesignerStatementTypes.acsCommercial
            } else {
                signer.statementType = AuthenticodesignerStatementTypes.acsIndividual
            }
                
            switch (timestampType) {
                case 1:
                    signer.timestampType = AuthenticodesignerTimestampTypes.actTrusted
                    signer.timestampServer = timestampServer
                    break
                case 2:
                    signer.timestampType = AuthenticodesignerTimestampTypes.actLegacy
                    signer.timestampServer = timestampServer
                    break
                default:
                    signer.timestampServer = ""
            }
            
            do {
                try signer.sign()
                showMessage = ShowMes(message: "The file successfully signed", title: "Info")
            }
            catch
            {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Sign").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
