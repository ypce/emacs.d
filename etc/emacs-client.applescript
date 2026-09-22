-- Emacs Client: open GUI frames on the launchd Emacs daemon.
-- No -a fallback: the daemon is managed by launchd (KeepAlive),
-- a client must never spawn a shadow daemon.

property emacsclient : "/opt/homebrew/opt/emacs-plus@31/bin/emacsclient"

on open theDropped
	repeat with oneDrop in theDropped
		set dropPath to quoted form of POSIX path of oneDrop
		try
			do shell script emacsclient & " -c -n " & dropPath
		end try
	end repeat
end open

on run
	try
		do shell script emacsclient & " -c -n"
	end try
end run

on open location this_URL
	try
		do shell script emacsclient & " -n " & quoted form of this_URL
	end try
end open location
