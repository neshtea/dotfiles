local org = require('orgmode')
org.setup_ts_grammar()
org.setup {
    org_agenda_files = { '~/Dropbox/Brain/Tasks/*' },
    org_default_notes_file = '~/Dropbox/Brain/Tasks/gtd.org',
}
