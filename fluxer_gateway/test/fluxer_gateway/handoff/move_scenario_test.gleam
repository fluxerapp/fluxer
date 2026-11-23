// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/message
import fluxer_gateway/handoff/arrival
import fluxer_gateway/handoff/source
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import support/handoff_sim.{
  AfterCommitWrite, AfterTailComplete, AtMs, BouncerCrash, Config, InstallDelay,
  InstallFails, OnApplied, OnBegin, OnInstalled, OnSeal, OnTailChunk, Refuse,
  SourceCrash, TargetCrash,
}

fn with_faults(faults: List(handoff_sim.Fault)) -> handoff_sim.Config {
  Config(..handoff_sim.config(), faults:)
}

fn assert_moved(sim) -> Nil {
  let assert Some(source.Moved(_, _, _)) = handoff_sim.outcome(sim)
  let assert Some(arrival.Arrived(_)) = handoff_sim.arrival_outcome(sim)
  assert handoff_sim.target_mode(sim) == handoff_sim.Open
  assert handoff_sim.final_ops(sim) == handoff_sim.all_ops(sim)
  assert handoff_sim.lost(sim) == []
  assert handoff_sim.serving_owners(sim) == 1
}

fn assert_thawed(sim) -> Nil {
  let assert Some(source.Thawed(_)) = handoff_sim.outcome(sim)
  assert handoff_sim.source_mode(sim) == handoff_sim.Serving
  assert handoff_sim.target_mode(sim) != handoff_sim.Open
  assert handoff_sim.final_ops(sim) == handoff_sim.all_ops(sim)
  assert handoff_sim.lost(sim) == []
  assert handoff_sim.serving_owners(sim) == 1
  assert !handoff_sim.committed(sim)
}

pub fn plain_move_keeps_every_message_in_order_test() {
  handoff_sim.run(handoff_sim.config())
  |> assert_moved
}

pub fn messages_during_a_long_freeze_are_forwarded_in_order_test() {
  handoff_sim.run(
    Config(..handoff_sim.config(), ops: 300, faults: [InstallDelay(150)]),
  )
  |> assert_moved
}

pub fn refused_admission_leaves_the_entity_serving_test() {
  let sim = handoff_sim.run(with_faults([Refuse(message.Budget)]))
  assert handoff_sim.outcome(sim)
    == Some(source.NotMoved(source.Refused(message.Budget)))
  assert handoff_sim.source_mode(sim) == handoff_sim.Serving
  assert handoff_sim.final_ops(sim) == handoff_sim.all_ops(sim)
  assert handoff_sim.target_mode(sim) == handoff_sim.Absent
}

pub fn home_dies_before_admission_retries_at_the_new_home_test() {
  handoff_sim.run(with_faults([BouncerCrash(0, OnBegin)]))
  |> assert_moved
}

pub fn home_dies_during_blob_streaming_resumes_at_the_successor_test() {
  handoff_sim.run(
    Config(..handoff_sim.config(), tail_padding: 1_500_000, faults: [
      BouncerCrash(0, OnTailChunk),
    ]),
  )
  |> assert_moved
}

pub fn home_dies_after_installed_before_commit_aborts_and_thaws_test() {
  let sim = handoff_sim.run(with_faults([BouncerCrash(0, OnInstalled)]))
  assert_thawed(sim)
  assert handoff_sim.arrival_outcome(sim) == Some(arrival.Discarded)
}

pub fn home_dies_after_tail_completes_aborts_and_thaws_test() {
  handoff_sim.run(with_faults([BouncerCrash(0, AfterTailComplete)]))
  |> assert_thawed
}

pub fn home_dies_after_the_commit_write_completes_the_move_test() {
  handoff_sim.run(
    Config(..handoff_sim.config(), ops: 200, faults: [
      BouncerCrash(0, AfterCommitWrite),
    ]),
  )
  |> assert_moved
}

pub fn home_dies_before_the_seal_completes_the_move_test() {
  handoff_sim.run(
    Config(..handoff_sim.config(), ops: 200, faults: [BouncerCrash(0, OnSeal)]),
  )
  |> assert_moved
}

pub fn home_dies_after_the_seal_reaches_the_target_completes_the_move_test() {
  handoff_sim.run(
    Config(..handoff_sim.config(), ops: 30, faults: [BouncerCrash(0, OnApplied)]),
  )
  |> assert_moved
}

pub fn home_and_successor_die_after_commit_complete_by_the_in_doubt_rule_test() {
  handoff_sim.run(
    Config(..handoff_sim.config(), ops: 200, faults: [
      BouncerCrash(0, AfterCommitWrite),
      BouncerCrash(1, AfterCommitWrite),
    ]),
  )
  |> assert_moved
}

pub fn home_and_successor_die_before_commit_abort_by_the_in_doubt_rule_test() {
  let sim =
    handoff_sim.run(
      with_faults([BouncerCrash(0, OnInstalled), BouncerCrash(1, OnInstalled)]),
    )
  assert_thawed(sim)
}

pub fn target_dies_before_installed_aborts_and_thaws_test() {
  let sim = handoff_sim.run(with_faults([TargetCrash(AfterTailComplete)]))
  assert_thawed(sim)
}

pub fn target_refusing_the_install_aborts_and_thaws_test() {
  let sim = handoff_sim.run(with_faults([InstallFails]))
  assert_thawed(sim)
  let assert Some(arrival.Refused(_)) = handoff_sim.arrival_outcome(sim)
}

pub fn slow_target_past_the_freeze_bound_aborts_and_thaws_test() {
  let sim = handoff_sim.run(with_faults([InstallDelay(3000)]))
  assert_thawed(sim)
  assert handoff_sim.arrival_outcome(sim) == Some(arrival.Discarded)
}

pub fn source_dies_after_the_blob_commits_without_its_tail_gap_test() {
  let sim =
    handoff_sim.run(
      Config(..handoff_sim.config(), ops: 100, op_every_ms: 1, faults: [
        SourceCrash(AfterTailComplete),
      ]),
    )
  assert handoff_sim.committed(sim)
  assert handoff_sim.target_mode(sim) == handoff_sim.Open
  let delivered = handoff_sim.final_ops(sim)
  let lost = handoff_sim.lost(sim)
  assert list.sort(list.append(delivered, lost), int.compare)
    == handoff_sim.all_ops(sim)
  assert is_increasing(delivered)
}

pub fn source_dies_before_the_blob_is_complete_aborts_test() {
  let sim =
    handoff_sim.run(
      Config(..handoff_sim.config(), tail_padding: 1_500_000, faults: [
        SourceCrash(OnTailChunk),
      ]),
    )
  assert handoff_sim.aborted(sim)
  assert !handoff_sim.committed(sim)
  assert handoff_sim.serving_owners(sim) == 0
  assert handoff_sim.target_mode(sim) != handoff_sim.Open
}

pub fn bouncer_majority_lost_while_in_doubt_fences_and_never_thaws_test() {
  let sim =
    handoff_sim.run(
      Config(..handoff_sim.config(), faults: [
        BouncerCrash(0, OnInstalled),
        BouncerCrash(1, AtMs(0)),
        BouncerCrash(2, AtMs(0)),
      ]),
    )
  assert handoff_sim.outcome(sim) == Some(source.Fenced)
  assert handoff_sim.source_mode(sim) == handoff_sim.SourceDead
  assert handoff_sim.serving_owners(sim) == 0
}

pub fn non_home_bouncer_loss_changes_nothing_test() {
  handoff_sim.run(with_faults([BouncerCrash(2, AtMs(8))]))
  |> assert_moved
}

fn is_increasing(values: List(Int)) -> Bool {
  case values {
    [a, b, ..rest] -> a < b && is_increasing([b, ..rest])
    _ -> True
  }
}

pub fn majority_commit_is_what_lets_a_new_home_finish_a_released_move_test() {
  let faults = [BouncerCrash(0, OnSeal)]
  handoff_sim.run(Config(..handoff_sim.config(), ops: 200, faults:))
  |> assert_moved
  let single =
    handoff_sim.run(
      Config(..handoff_sim.config(), ops: 200, faults:, commit_quorum_one: True),
    )
  assert handoff_sim.outcome(single) == None
  assert handoff_sim.source_mode(single) == handoff_sim.Direct
  assert handoff_sim.target_mode(single) == handoff_sim.Arriving
  assert handoff_sim.serving_owners(single) == 0
  assert handoff_sim.final_ops(single) == []
}

pub fn precopied_move_keeps_every_message_in_order_test() {
  handoff_sim.run(
    Config(..handoff_sim.config(), ops: 120, precopy: Some(Ok(40))),
  )
  |> assert_moved
}

pub fn failed_precopy_still_moves_with_a_full_freeze_test() {
  handoff_sim.run(Config(..handoff_sim.config(), precopy: Some(Error(20))))
  |> assert_moved
}

pub fn precopy_past_the_freeze_bound_thaws_and_keeps_serving_test() {
  let sim =
    handoff_sim.run(
      Config(..handoff_sim.config(), precopy: Some(Ok(30)), faults: [
        InstallDelay(3000),
      ]),
    )
  assert_thawed(sim)
}

fn copying_source() -> source.Source {
  let machine =
    source.new(
      move: handoff_sim.move_id(),
      key: handoff_sim.entity(),
      epoch: epoch.zero(),
      to: handoff_sim.target_worker(),
      version: 3,
      estimate: 0,
      timings: source.default_timings(),
      precopy: True,
    )
  let #(machine, _) = source.step(machine, source.Begin, 0)
  let #(machine, effects) =
    source.step(
      machine,
      source.FromHome(message.MoveAccepted(handoff_sim.move_id())),
      1,
    )
  assert effects == [source.RunPrecopy, source.After(source.LeaseTimer, 250)]
  assert source.phase(machine) == source.Copying
  machine
}

pub fn precopy_runs_before_the_freeze_and_the_freeze_clock_starts_after_it_test() {
  let #(machine, effects) =
    source.step(copying_source(), source.PrecopyDone(Ok(2)), 900)
  assert effects == [source.RunFreeze]
  assert source.phase(machine) == source.Freezing(False)
}

pub fn an_abort_while_copying_leaves_the_entity_serving_test() {
  let #(machine, effects) =
    source.step(
      copying_source(),
      source.FromHome(message.Aborted(handoff_sim.move_id())),
      5,
    )
  assert effects
    == [source.Abandon, source.Done(source.NotMoved(source.AbortedEarly))]
  assert source.is_finished(machine)
  let #(_, effects) = source.step(copying_source(), source.LeaseLapsed, 5)
  assert effects
    == [source.Abandon, source.Done(source.NotMoved(source.LeaseLost))]
}
