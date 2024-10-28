import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var writer = ArchiveWriter()

    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    @State private var archiveType = 0
    @State private var archiveFile: String = ""
    @State private var inputFiles: String = ""
    @State private var output: String = ""
    @State private var showMessage: ShowMes?
        
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
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)

            }
            
            
            Text("Input filenames:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("filename;folder1;folder2;filename2", text: $inputFiles)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)

            }
            
            
            createButton()
            Text("Output:")
            TextEditor(text: $output)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                .border(Color.black, width: 1)
                
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }
    
    @ViewBuilder
    private func createButton() -> some View {
        Button(action: {
            //writer.runtimeLicense = ""
            showMessage = nil
            output = ""
            do {
                writer.compressionLevel = 6
                writer.encryptionType = ArchivewriterEncryptionTypes.aetNoEncryption
                switch archiveType {
                    case 1:
                        try writer.createNew(archiveType: ArchivewriterArchiveTypes.aftTarGzip.rawValue)
                        break;
                    case 2:
                        try writer.createNew(archiveType: ArchivewriterArchiveTypes.aftTarBzip2.rawValue)
                        break;
                    default:
                        try writer.createNew(archiveType: ArchivewriterArchiveTypes.aftZip.rawValue)
                }
                
                let files = inputFiles.components(separatedBy: ";")
                for file in files {
                    if (file != "") {
                        file = documentsPath + file
                        let fileManager = FileManager.default
                        var isDir : ObjCBool = false
                        if (fileManager.fileExists(atPath: file, isDirectory: &isDir)) {
                            if (isDir.boolValue) {
                                try writer.addFiles(folder: "", localPath: file, recursive: true)
                            } else {
                                try _ = writer.addFile(path: (file as NSString).lastPathComponent, localPath: file)
                            }
                        }
                    }
                }
                
                if (writer.files.count > 0) {
                    for file in writer.files {
                        output += file.path + "\n"
                    }
                    
                    try writer.save(archiveName: documentsPath + archiveFile)
                    try writer.close()
                    showMessage = ShowMes(message: "Archive successfully created", title: "Info")
                } else {
                    try writer.close()
                    showMessage = ShowMes(message: "Can't create archive without files")
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
                Text("Create archive").font(.system(size: 20))
                    .frame(minWidth: 150, minHeight: 40)
                    .background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
