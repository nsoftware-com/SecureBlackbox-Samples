import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var reader = ArchiveReader()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/";
    
    @State private var archiveType = 0
    @State private var archiveFile: String = ""
    @State private var output: String = ""
    @State private var showMessage: ShowMes?
    @State private var bExtractEnabled = false
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Archive type:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Picker(selection: $archiveType, label: Text("")) {
                Text("Zip")
                    .tag(0)
                Text("Tar Gzip")
                    .tag(1)
                Text("Tar Bzip2")
                    .tag(2)
            }
            .pickerStyle(SegmentedPickerStyle())
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            
            Text("Archive filename:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
                TextField("Enter archive filename", text: $archiveFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openArchiveButton()
            }
            
            
            readerButton()
            Text("Output:")
            TextEditor(text: $output)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                .border(Color.black, width: 1)
            extractorButton()

        }
        .padding(.all, 8)
        .alert(item: $showMessage) {
            Alert(title: Text($0.title), message: Text($0.message), dismissButton: .default(Text("Ok")))
        }
    }
    
    @ViewBuilder
    private func openArchiveButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title = "Choose an archive file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    archiveFile = result!.path
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
    private func readerButton() -> some View {
        Button(action: {
            //reader.runtimeLicense = ""
            showMessage = nil
            output = ""
            do {
                if (reader.opened == true) {
                    try reader.close()
                }
                let archivePath = archiveFile
                switch archiveType {
                    case 1:
                        try reader.open(archiveType: ArchivereaderArchiveTypes.aftTarGzip.rawValue, archivePath:  archivePath)
                        break;
                    case 2:
                        try reader.open(archiveType: ArchivereaderArchiveTypes.aftTarBzip2.rawValue, archivePath: archivePath)
                        break;
                    default:
                        try reader.open(archiveType: ArchivereaderArchiveTypes.aftZip.rawValue, archivePath: archivePath)
                }
                bExtractEnabled = true
                
                for file in reader.files {
                    output += file.path + "\n"
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Open").font(.system(size: 20))
                .frame(minWidth: 150, minHeight: 40)
                .background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func extractorButton() -> some View {
        Button(action: {
            showMessage = nil
            do {
                let dialog = NSOpenPanel()
                dialog.title = "Select folder for extracting files"
                dialog.showsResizeIndicator = true
                dialog.showsHiddenFiles = false
                dialog.allowsMultipleSelection = false
                dialog.canChooseFiles = false
                dialog.canChooseDirectories = true

                if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                    if (dialog.url != nil) {
                        try reader.extractAll(outputPath: dialog.url!.path + "/", overwriteExisting: true)
                        showMessage = ShowMes(message: "All files successfully extracted", title: "Info")
                    }
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Extract all").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle()).disabled(bExtractEnabled == false)
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
