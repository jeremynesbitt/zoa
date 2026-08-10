submodule (codeV_commands) mod_codev_undoredo
implicit none
contains

    ! UNDO: restore the previous lens snapshot.
    !## cmd:      UNDO
    !## syntax:   UNDO
    !## category: Lens System Commands
    !## desc:     Undo the last lens change.
    !##
    module procedure execUndo
        use undo_manager, only: undo_undo
        call undo_undo()
    end procedure execUndo

    ! REDO: re-apply the next lens snapshot (after one or more UNDOs).
    !## cmd:      REDO
    !## syntax:   REDO
    !## category: Lens System Commands
    !## desc:     Redo the last undone lens change.
    !##
    module procedure execRedo
        use undo_manager, only: undo_redo
        call undo_redo()
    end procedure execRedo

end submodule mod_codev_undoredo
