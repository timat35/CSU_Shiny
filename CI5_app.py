

"""Premier exemple avec Tkinter.

On crée une fenêtre simple qui souhaite la bienvenue à l'utilisateur.

"""

# On importe Tkinter
from functools import partial
from tkinter import *
import tkinter.filedialog
import sys
import os
import pickle
import subprocess

def choose_file(input,picklePath):
   
    file = tkinter.filedialog.askopenfilename (parent=root,title='Choose a file')
    input.delete(0,END)
    input.insert(0,file)
    file_pickle = open(pickle_filepath, 'wb')
    d = {'Rpath' : file}
    pickle.dump(d, file_pickle,protocol=pickle.HIGHEST_PROTOCOL) 
	
def launch_CI5_shiny(input,app):

    app_script = (input.get() + ' "' + app + '"'  )
    subpr = subprocess.Popen(app_script)
    root.quit()

doc_path = os.path.expanduser(r'~\Documents')
	
pickle_filepath = doc_path + '/Rfolder.pickle'

d = {'Rpath' : "R installation path/bin/Rscript.exe"}

if not os.path.exists(pickle_filepath):
    file_pickle = open(pickle_filepath, 'wb')
    pickle.dump(d, file_pickle)  
    Rpath = d['Rpath']
else:
    file_pickle = open(pickle_filepath, 'rb') 
    temp_d = pickle.load(file_pickle) 
    Rpath = temp_d['Rpath']

		
		
root = Tk()
root.title('CI5 graph generator')

labelRpath = Label(root, text='R path')

value = StringVar() 
value.set(Rpath)
inputRpath = Entry(root, textvariable=value, width=50)

app_temp = '\\\\inti\CIN\Xchange\Mathieu\CI5_shiny\CI5_registry_graph.R'

buttonChoosePath = Button(root, text='Select R 3.4.x file', command=partial(choose_file, inputRpath,pickle_filepath))
buttonLaunchApp = Button(root, text='Launch CI5 graph', command=partial(launch_CI5_shiny, inputRpath,app_temp))

labelRpath.grid(row=1,column=1,sticky=W,padx=10,pady=10)
inputRpath.grid(row=1,column=2,padx=10,pady=10)
buttonChoosePath.grid(row=1, column=3, sticky=W,padx=10,pady=10)
buttonLaunchApp.grid(row=2, column=2, sticky=N,padx=10,pady=5)

root.mainloop()