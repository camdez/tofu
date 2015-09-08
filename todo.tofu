{:version 2,
 :tasks
 [{:name
   "Memoize filtered tasks (or note how we generated them to avoid regen)",
   :created #inst "2014-01-15T19:29:18.134-00:00",
   :completed #inst "2014-01-28T06:48:25.933-00:00"}
  {:name "Basic sort (toggle command)",
   :created #inst "2014-01-15T07:31:25.683-00:00",
   :completed #inst "2014-01-16T16:53:26.449-00:00"}
  {:name "Basic filter (toggle command)",
   :created #inst "2014-01-15T07:31:34.941-00:00",
   :completed #inst "2014-01-18T05:30:55.339-00:00"}
  {:priority false,
   :name "Add undo support",
   :created #inst "2014-01-15T16:13:30.852-00:00",
   :completed false}
  {:priority false,
   :name "Rework `choose-task` to be value-based",
   :created #inst "2014-01-15T16:16:40.967-00:00",
   :completed #inst "2014-01-15T18:44:11.323-00:00"}
  {:priority false,
   :name "Add ability to clear the screen",
   :created #inst "2014-01-15T16:31:21.031-00:00",
   :completed #inst "2014-01-15T16:31:26.256-00:00"}
  {:name "Organize project source",
   :created #inst "2014-01-15T18:14:26.643-00:00",
   :completed #inst "2014-01-24T04:33:02.527-00:00"}
  {:name "Consider using RPN-style (stack-based) filter management",
   :created #inst "2014-01-16T16:54:23.917-00:00",
   :completed false}
  {:name "Regex filtering",
   :created #inst "2014-01-16T16:54:37.867-00:00",
   :completed #inst "2014-01-16T16:54:47.729-00:00"}
  {:priority false,
   :name "Automatically save",
   :created #inst "2014-01-16T16:55:13.390-00:00",
   :completed false}
  {:name "Add notes field",
   :created #inst "2014-01-16T16:55:18.253-00:00",
   :completed false}
  {:priority true,
   :name "Add indication that the task list is filtered",
   :created #inst "2014-01-16T16:55:28.761-00:00",
   :completed #inst "2014-01-24T07:03:59.699-00:00"}
  {:priority true,
   :name "Add stats",
   :created #inst "2014-01-16T16:55:35.275-00:00",
   :completed false}
  {:priority true,
   :name "Add starring or priorities",
   :created #inst "2014-01-16T16:57:00.093-00:00",
   :completed #inst "2014-01-24T05:48:58.627-00:00"}
  {:name "Allow editing of task 'name'",
   :created #inst "2014-01-16T16:59:00.481-00:00",
   :completed #inst "2014-01-28T05:53:13.543-00:00"}
  {:name
   "Grouping? By date (upcoming, etc.)? By first tag? (like a project)",
   :created #inst "2014-01-16T17:00:13.353-00:00",
   :completed false}
  {:name "Add ability to run action on multiple tasks at once",
   :created #inst "2014-01-24T05:49:35.280-00:00",
   :completed false}
  {:priority true,
   :name "Multiple file support",
   :created #inst "2014-01-24T05:49:47.644-00:00",
   :completed #inst "2015-08-29T16:19:31.648-00:00"}
  {:name "Output what settings changes are doing as they happen",
   :created #inst "2014-01-24T05:50:15.297-00:00",
   :completed #inst "2014-01-25T05:34:53.936-00:00"}
  {:priority true,
   :name "Add a basic help command (print contents of command map)",
   :created #inst "2014-01-24T05:51:30.043-00:00",
   :completed #inst "2014-01-25T06:12:09.452-00:00"}
  {:name "Add tagging",
   :created #inst "2014-01-24T06:08:45.325-00:00",
   :completed false}
  {:name "File format versioning (simple)",
   :created #inst "2014-01-24T06:08:53.746-00:00",
   :completed #inst "2014-01-28T06:03:33.724-00:00"}
  {:priority true,
   :name "Saved presets",
   :created #inst "2014-01-24T06:09:17.178-00:00",
   :completed #inst "2015-09-08T02:30:00.386-00:00"}
  {:priority true,
   :name "Show task ages",
   :created #inst "2014-01-28T06:08:36.839-00:00",
   :completed #inst "2014-03-14T22:57:04.579-00:00"}
  {:priority true,
   :name "Show recently done?",
   :created #inst "2014-01-28T06:51:06.369-00:00",
   :completed false}
  {:name "Pagination?",
   :created #inst "2014-01-28T19:27:28.849-00:00",
   :completed false}
  {:name "Testing task ages",
   :created #inst "2014-03-14T22:45:49.195-00:00",
   :completed #inst "2014-03-15T01:13:21.595-00:00"}
  {:name "Persist preferences",
   :created #inst "2014-03-14T22:58:47.148-00:00",
   :completed #inst "2015-09-08T04:14:15.640-00:00"}
  {:name "Priorities (and priority filtering)",
   :created #inst "2014-03-14T22:59:08.462-00:00",
   :completed false}
  {:priority true,
   :name "Reverse sort order",
   :created #inst "2014-03-14T22:59:25.680-00:00",
   :completed #inst "2014-10-10T03:01:41.795-00:00"}
  {:name "Add support for due dates",
   :created #inst "2014-03-14T22:59:37.921-00:00",
   :completed false}
  {:name "i18n support (just think ahead)",
   :created #inst "2014-03-14T23:00:13.396-00:00",
   :completed false}
  {:priority true,
   :name "Show completion dates",
   :created #inst "2014-03-14T23:01:33.380-00:00",
   :completed false}
  {:name "Search (basically a non-persisted filter)",
   :created #inst "2014-03-14T23:01:56.910-00:00",
   :completed false}
  {:name "Consider an auto (screen) clearing option",
   :created #inst "2014-06-20T00:13:32.629-00:00",
   :completed false}
  {:priority true,
   :name "Cancel adding task if no text is entered",
   :created #inst "2014-06-20T00:13:48.109-00:00",
   :completed #inst "2014-10-10T02:49:34.152-00:00"}
  {:name "Test task ages",
   :created #inst "2014-10-10T02:41:01.689-00:00",
   :completed #inst "2014-10-10T02:42:23.882-00:00"}
  {:name "Separate functional / io code",
   :created #inst "2015-08-29T15:40:49.831-00:00",
   :completed false}
  {:name
   "Task registers? (e.g. assignable letters for quick reference)",
   :created #inst "2015-08-29T15:41:38.730-00:00",
   :completed false}],
 :opts {:name-filter nil, :hide-done true, :sort-mode 0}}
