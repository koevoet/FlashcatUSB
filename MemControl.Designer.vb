<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MemControl
    Inherits System.Windows.Forms.UserControl

    'UserControl overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.gbMemory = New System.Windows.Forms.GroupBox()
        Me.lblMemName = New System.Windows.Forms.Label()
        Me.AddrBox = New System.Windows.Forms.GroupBox()
        Me.txtAddress = New System.Windows.Forms.TextBox()
        Me.gbFileIO = New System.Windows.Forms.GroupBox()
        Me.cmdMemWrite = New System.Windows.Forms.Button()
        Me.cmdMemRead = New System.Windows.Forms.Button()
        Me.FlashTransfer = New System.Windows.Forms.GroupBox()
        Me.lblSpeed = New System.Windows.Forms.Label()
        Me.pbar = New System.Windows.Forms.ProgressBar()
        Me.MemHexEditor = New FlashcatUSB.HexEditor()
        Me.gbMemory.SuspendLayout()
        Me.AddrBox.SuspendLayout()
        Me.gbFileIO.SuspendLayout()
        Me.FlashTransfer.SuspendLayout()
        Me.SuspendLayout()
        '
        'gbMemory
        '
        Me.gbMemory.Controls.Add(Me.lblMemName)
        Me.gbMemory.Location = New System.Drawing.Point(2, 4)
        Me.gbMemory.Name = "gbMemory"
        Me.gbMemory.Size = New System.Drawing.Size(149, 47)
        Me.gbMemory.TabIndex = 13
        Me.gbMemory.TabStop = False
        Me.gbMemory.Text = "(not set) device"
        '
        'lblMemName
        '
        Me.lblMemName.AutoSize = True
        Me.lblMemName.Location = New System.Drawing.Point(6, 21)
        Me.lblMemName.Name = "lblMemName"
        Me.lblMemName.Size = New System.Drawing.Size(67, 13)
        Me.lblMemName.TabIndex = 0
        Me.lblMemName.Text = "(Not labeled)"
        '
        'AddrBox
        '
        Me.AddrBox.Controls.Add(Me.txtAddress)
        Me.AddrBox.Location = New System.Drawing.Point(323, 4)
        Me.AddrBox.Name = "AddrBox"
        Me.AddrBox.Size = New System.Drawing.Size(109, 47)
        Me.AddrBox.TabIndex = 12
        Me.AddrBox.TabStop = False
        Me.AddrBox.Text = "Address"
        '
        'txtAddress
        '
        Me.txtAddress.Location = New System.Drawing.Point(6, 18)
        Me.txtAddress.Name = "txtAddress"
        Me.txtAddress.Size = New System.Drawing.Size(92, 20)
        Me.txtAddress.TabIndex = 5
        '
        'gbFileIO
        '
        Me.gbFileIO.Controls.Add(Me.cmdMemWrite)
        Me.gbFileIO.Controls.Add(Me.cmdMemRead)
        Me.gbFileIO.Location = New System.Drawing.Point(158, 4)
        Me.gbFileIO.Name = "gbFileIO"
        Me.gbFileIO.Size = New System.Drawing.Size(158, 47)
        Me.gbFileIO.TabIndex = 11
        Me.gbFileIO.TabStop = False
        Me.gbFileIO.Text = "File I/O"
        '
        'cmdMemWrite
        '
        Me.cmdMemWrite.Location = New System.Drawing.Point(81, 19)
        Me.cmdMemWrite.Name = "cmdMemWrite"
        Me.cmdMemWrite.Size = New System.Drawing.Size(69, 22)
        Me.cmdMemWrite.TabIndex = 2
        Me.cmdMemWrite.Text = "Write"
        Me.cmdMemWrite.UseVisualStyleBackColor = True
        '
        'cmdMemRead
        '
        Me.cmdMemRead.Location = New System.Drawing.Point(6, 19)
        Me.cmdMemRead.Name = "cmdMemRead"
        Me.cmdMemRead.Size = New System.Drawing.Size(69, 22)
        Me.cmdMemRead.TabIndex = 1
        Me.cmdMemRead.Text = "Read"
        Me.cmdMemRead.UseVisualStyleBackColor = True
        '
        'FlashTransfer
        '
        Me.FlashTransfer.Controls.Add(Me.lblSpeed)
        Me.FlashTransfer.Location = New System.Drawing.Point(323, 4)
        Me.FlashTransfer.Name = "FlashTransfer"
        Me.FlashTransfer.Size = New System.Drawing.Size(109, 47)
        Me.FlashTransfer.TabIndex = 14
        Me.FlashTransfer.TabStop = False
        Me.FlashTransfer.Text = "Transfer Speed"
        Me.FlashTransfer.Visible = False
        '
        'lblSpeed
        '
        Me.lblSpeed.AutoSize = True
        Me.lblSpeed.Location = New System.Drawing.Point(8, 21)
        Me.lblSpeed.Name = "lblSpeed"
        Me.lblSpeed.Size = New System.Drawing.Size(0, 13)
        Me.lblSpeed.TabIndex = 1
        '
        'pbar
        '
        Me.pbar.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.pbar.Location = New System.Drawing.Point(3, 292)
        Me.pbar.Name = "pbar"
        Me.pbar.Size = New System.Drawing.Size(427, 12)
        Me.pbar.TabIndex = 15
        '
        'MemHexEditor
        '
        Me.MemHexEditor.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.MemHexEditor.Location = New System.Drawing.Point(0, 53)
        Me.MemHexEditor.Name = "MemHexEditor"
        Me.MemHexEditor.Size = New System.Drawing.Size(434, 237)
        Me.MemHexEditor.TabIndex = 10
        '
        'MemControl
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.pbar)
        Me.Controls.Add(Me.gbMemory)
        Me.Controls.Add(Me.gbFileIO)
        Me.Controls.Add(Me.MemHexEditor)
        Me.Controls.Add(Me.AddrBox)
        Me.Controls.Add(Me.FlashTransfer)
        Me.Name = "MemControl"
        Me.Size = New System.Drawing.Size(437, 308)
        Me.gbMemory.ResumeLayout(False)
        Me.gbMemory.PerformLayout()
        Me.AddrBox.ResumeLayout(False)
        Me.AddrBox.PerformLayout()
        Me.gbFileIO.ResumeLayout(False)
        Me.FlashTransfer.ResumeLayout(False)
        Me.FlashTransfer.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents gbMemory As System.Windows.Forms.GroupBox
    Friend WithEvents lblMemName As System.Windows.Forms.Label
    Friend WithEvents AddrBox As System.Windows.Forms.GroupBox
    Friend WithEvents txtAddress As System.Windows.Forms.TextBox
    Friend WithEvents gbFileIO As System.Windows.Forms.GroupBox
    Friend WithEvents cmdMemWrite As System.Windows.Forms.Button
    Friend WithEvents cmdMemRead As System.Windows.Forms.Button
    Friend WithEvents FlashTransfer As System.Windows.Forms.GroupBox
    Friend WithEvents lblSpeed As System.Windows.Forms.Label
    Friend WithEvents MemHexEditor As FlashcatUSB.HexEditor
    Friend WithEvents pbar As System.Windows.Forms.ProgressBar

End Class
