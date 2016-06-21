VERSION 5.00
Begin VB.Form AdviceForm 
   Caption         =   "Advice"
   ClientHeight    =   2715
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   3465
   LinkTopic       =   "Form1"
   ScaleHeight     =   2715
   ScaleWidth      =   3465
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox Advice 
      Height          =   285
      Left            =   120
      TabIndex        =   9
      Top             =   2280
      Width           =   3135
   End
   Begin VB.CommandButton GetAdvice 
      Caption         =   "Get Advice"
      Height          =   375
      Left            =   120
      TabIndex        =   8
      Top             =   1680
      Width           =   1215
   End
   Begin VB.ComboBox BallFlight 
      Height          =   315
      ItemData        =   "advice.frx":0000
      Left            =   1440
      List            =   "advice.frx":000D
      Style           =   2  'Dropdown List
      TabIndex        =   7
      Top             =   1200
      Width           =   1815
   End
   Begin VB.ComboBox Favor 
      Height          =   315
      ItemData        =   "advice.frx":0024
      Left            =   1440
      List            =   "advice.frx":002E
      Style           =   2  'Dropdown List
      TabIndex        =   5
      Top             =   840
      Width           =   1815
   End
   Begin VB.ComboBox ClubType 
      Height          =   315
      ItemData        =   "advice.frx":0046
      Left            =   1440
      List            =   "advice.frx":005C
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   480
      Width           =   1815
   End
   Begin VB.TextBox SwingSpeed 
      Height          =   285
      Left            =   1440
      TabIndex        =   1
      Top             =   120
      Width           =   615
   End
   Begin VB.Label Label5 
      Caption         =   "mph"
      Height          =   255
      Left            =   2160
      TabIndex        =   10
      Top             =   120
      Width           =   615
   End
   Begin VB.Label Label4 
      Caption         =   "Ball Flight:"
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   1200
      Width           =   1095
   End
   Begin VB.Label Label3 
      Caption         =   "Favor:"
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   840
      Width           =   1095
   End
   Begin VB.Label Label2 
      Caption         =   "Club Type:"
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   480
      Width           =   1095
   End
   Begin VB.Label Label1 
      Caption         =   "Swing Speed:"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   1095
   End
End
Attribute VB_Name = "AdviceForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub GetAdvice_Click()
    Dim arxl As New ARulesXL
    Dim rulesetfile, arulespath As String
    
    ' Open the ruleset
    ChDir App.path
    rulesetfile = App.path + "\advice.axl"
    arulespath = App.path + "\"
    Call arxl.OpenRules(rulesetfile, arulespath)
    
    ' Load the user inputs
    Call arxl.ClearVector("ShaftRules", "in")
    Call arxl.AddToVector("ShaftRules", "in", "Swing Speed", SwingSpeed.Text)
    Call arxl.AddToVector("ShaftRules", "in", "Club Type", ClubType.Text)
    Call arxl.AddToVector("ShaftRules", "in", "Favor", Favor.Text)
    Call arxl.AddToVector("ShaftRules", "in", "Ball Flight", BallFlight.Text)
    
    ' Get the advice
    Advice.Text = arxl.QueryRules("ShaftRules", "FIND advice")
    
End Sub
