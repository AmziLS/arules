<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class AdviceForm
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
      Me.SwingSpeed = New System.Windows.Forms.TextBox
      Me.Label1 = New System.Windows.Forms.Label
      Me.ClubType = New System.Windows.Forms.ListBox
      Me.Label2 = New System.Windows.Forms.Label
      Me.Favor = New System.Windows.Forms.ListBox
      Me.Label3 = New System.Windows.Forms.Label
      Me.BallFlight = New System.Windows.Forms.ListBox
      Me.Label4 = New System.Windows.Forms.Label
      Me.GoButton = New System.Windows.Forms.Button
      Me.Label5 = New System.Windows.Forms.Label
      Me.Advice = New System.Windows.Forms.TextBox
      Me.SuspendLayout()
      '
      'SwingSpeed
      '
      Me.SwingSpeed.Location = New System.Drawing.Point(115, 6)
      Me.SwingSpeed.Name = "SwingSpeed"
      Me.SwingSpeed.Size = New System.Drawing.Size(74, 20)
      Me.SwingSpeed.TabIndex = 0
      '
      'Label1
      '
      Me.Label1.AutoSize = True
      Me.Label1.Location = New System.Drawing.Point(12, 13)
      Me.Label1.Name = "Label1"
      Me.Label1.Size = New System.Drawing.Size(73, 13)
      Me.Label1.TabIndex = 1
      Me.Label1.Text = "Swing Speed:"
      '
      'ClubType
      '
      Me.ClubType.FormattingEnabled = True
      Me.ClubType.Items.AddRange(New Object() {"Driver < 11 Degrees", "Driver >= 11 Degrees", "Fairway Wood", "Hybrid/Utility", "Iron", "Wedge"})
      Me.ClubType.Location = New System.Drawing.Point(115, 39)
      Me.ClubType.Name = "ClubType"
      Me.ClubType.Size = New System.Drawing.Size(136, 17)
      Me.ClubType.TabIndex = 2
      '
      'Label2
      '
      Me.Label2.AutoSize = True
      Me.Label2.Location = New System.Drawing.Point(12, 43)
      Me.Label2.Name = "Label2"
      Me.Label2.Size = New System.Drawing.Size(31, 13)
      Me.Label2.TabIndex = 3
      Me.Label2.Text = "Club:"
      '
      'Favor
      '
      Me.Favor.FormattingEnabled = True
      Me.Favor.Items.AddRange(New Object() {"Distance", "Accuracy"})
      Me.Favor.Location = New System.Drawing.Point(117, 71)
      Me.Favor.Name = "Favor"
      Me.Favor.Size = New System.Drawing.Size(134, 17)
      Me.Favor.TabIndex = 4
      '
      'Label3
      '
      Me.Label3.AutoSize = True
      Me.Label3.Location = New System.Drawing.Point(12, 75)
      Me.Label3.Name = "Label3"
      Me.Label3.Size = New System.Drawing.Size(37, 13)
      Me.Label3.TabIndex = 5
      Me.Label3.Text = "Favor:"
      '
      'BallFlight
      '
      Me.BallFlight.FormattingEnabled = True
      Me.BallFlight.Items.AddRange(New Object() {"High", "Normal", "Low"})
      Me.BallFlight.Location = New System.Drawing.Point(118, 108)
      Me.BallFlight.Name = "BallFlight"
      Me.BallFlight.Size = New System.Drawing.Size(133, 17)
      Me.BallFlight.TabIndex = 6
      '
      'Label4
      '
      Me.Label4.AutoSize = True
      Me.Label4.Location = New System.Drawing.Point(12, 112)
      Me.Label4.Name = "Label4"
      Me.Label4.Size = New System.Drawing.Size(55, 13)
      Me.Label4.TabIndex = 7
      Me.Label4.Text = "Ball Flight:"
      '
      'GoButton
      '
      Me.GoButton.Location = New System.Drawing.Point(312, 133)
      Me.GoButton.Name = "GoButton"
      Me.GoButton.Size = New System.Drawing.Size(86, 24)
      Me.GoButton.TabIndex = 8
      Me.GoButton.Text = "Get Advice"
      Me.GoButton.UseVisualStyleBackColor = True
      '
      'Label5
      '
      Me.Label5.AutoSize = True
      Me.Label5.Location = New System.Drawing.Point(12, 188)
      Me.Label5.Name = "Label5"
      Me.Label5.Size = New System.Drawing.Size(43, 13)
      Me.Label5.TabIndex = 9
      Me.Label5.Text = "Advice:"
      '
      'Advice
      '
      Me.Advice.Location = New System.Drawing.Point(118, 181)
      Me.Advice.Name = "Advice"
      Me.Advice.ReadOnly = True
      Me.Advice.Size = New System.Drawing.Size(280, 20)
      Me.Advice.TabIndex = 10
      '
      'AdviceForm
      '
      Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
      Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
      Me.ClientSize = New System.Drawing.Size(410, 221)
      Me.Controls.Add(Me.Advice)
      Me.Controls.Add(Me.Label5)
      Me.Controls.Add(Me.GoButton)
      Me.Controls.Add(Me.Label4)
      Me.Controls.Add(Me.BallFlight)
      Me.Controls.Add(Me.Label3)
      Me.Controls.Add(Me.Favor)
      Me.Controls.Add(Me.Label2)
      Me.Controls.Add(Me.ClubType)
      Me.Controls.Add(Me.Label1)
      Me.Controls.Add(Me.SwingSpeed)
      Me.Name = "AdviceForm"
      Me.Text = "Advice"
      Me.ResumeLayout(False)
      Me.PerformLayout()

   End Sub
    Friend WithEvents SwingSpeed As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ClubType As System.Windows.Forms.ListBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Favor As System.Windows.Forms.ListBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents BallFlight As System.Windows.Forms.ListBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents GoButton As System.Windows.Forms.Button
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Advice As System.Windows.Forms.TextBox

End Class
