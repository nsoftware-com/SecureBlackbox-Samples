import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var verifier = MessageTimestampVerifier()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var detached = false
    @State private var outpurFileLabel: String = "Output file:"
    @State private var outputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var showMessage: ShowMes?
                
    func detachedChange() -> String {
        if (detached) {
            return "Data file:"
        } else {
            return "Output file:"
        }
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
            
            
            Toggle(isOn: $detached) {
                Text("Detached")
            }
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
              
            Text("\(detachedChange())")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $outputFile)
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
            //verifier.runtimeLicense = ""
            showMessage = nil
            verifier.inputFile = documentsPath + inputFile
            let outputPath = documentsPath + outputFile
            do {
                if (detached) {
                    verifier.dataFile = outputPath
                    try verifier.verifyDetached()
                } else {
                    verifier.outputFile = outputPath
                    try verifier.verify()
                }
            
                switch (verifier.signatureValidationResult) {
                    case MessagetimestampverifierSignatureValidationResults.svtValid:
                        showMessage = ShowMes(message: "Signature validated successfully", title: "Info")
                        break
                    case MessagetimestampverifierSignatureValidationResults.svtCorrupted:
                        showMessage = ShowMes(message: "Signature is invalid", title: "Info")
                        break
                    case MessagetimestampverifierSignatureValidationResults.svtSignerNotFound:
                        showMessage = ShowMes(message: "Signer not found", title: "Info")
                        break
                    case MessagetimestampverifierSignatureValidationResults.svtFailure:
                        showMessage = ShowMes(message: "Signature verification failed", title: "Info")
                        break
                    default:
                        showMessage = ShowMes(message: "Unknown", title: "Info")
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Verify").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
