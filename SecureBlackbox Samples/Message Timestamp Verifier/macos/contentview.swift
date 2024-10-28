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
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openInputButton()
            }
            
            
            Toggle(isOn: $detached) {
                Text("Detached")
            }
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
              
            Text("\(detachedChange())")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $outputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            opensaveOutputButton()
            }
            
            
            verifyButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func openInputButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title = "Choose an input file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    inputFile = result!.path
                }
            }
        }, label: {
            Text("...").font(.system(size: 18))
                .frame(minWidth: 20, minHeight: 20)
                .background(RoundedRectangle(cornerRadius: 2).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func opensaveOutputButton() -> some View {
        Button(action: {
            var dialog: NSSavePanel
            if (detached) {
                dialog = NSOpenPanel()
                dialog.title = "Choose an input file"
                dialog.showsResizeIndicator = true
                dialog.showsHiddenFiles = false
                (dialog as! NSOpenPanel).allowsMultipleSelection = true
                (dialog as! NSOpenPanel).canChooseDirectories = true
            } else {
                dialog = NSSavePanel()
                dialog.title = "Choose an output file"
                dialog.showsResizeIndicator = true
                dialog.showsHiddenFiles = false
            }

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    outputFile = result!.path
                }
            }
        }, label: {
            Text("...").font(.system(size: 18))
                .frame(minWidth: 20, minHeight: 20)
                .background(RoundedRectangle(cornerRadius: 2).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }

    @ViewBuilder
    private func verifyButton() -> some View {
        Button(action: {
            //verifier.runtimeLicense = ""
            showMessage = nil
            verifier.inputFile = inputFile
            let outputPath = outputFile
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
