// SPDX-License-Identifier: AGPL-3.0-or-later

import isEqual from 'lodash/isEqual';
import {useCallback, useLayoutEffect, useRef} from 'react';
import type {FieldValues, UseFormReturn} from 'react-hook-form';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type RemoteFormIdentityKey = string | number | boolean | null | undefined;

export type RemoteFormAutomaticResetReason = 'initial' | 'identity-change' | 'remote-clean';
export type RemoteFormResetReason = RemoteFormAutomaticResetReason | 'commit' | 'explicit-reset';

export interface RemoteFormResetOptions {
	readonly keepDefaultValues?: boolean;
	readonly keepDirty?: boolean;
	readonly keepDirtyValues?: boolean;
	readonly keepErrors?: boolean;
	readonly keepIsSubmitted?: boolean;
	readonly keepIsSubmitSuccessful?: boolean;
	readonly keepIsValid?: boolean;
	readonly keepIsValidating?: boolean;
	readonly keepSubmitCount?: boolean;
	readonly keepTouched?: boolean;
	readonly keepValues?: boolean;
}

export interface RemoteFormResetDecisionInput {
	readonly hasAppliedRemoteValues: boolean;
	readonly identityChanged: boolean;
	readonly remoteValuesChanged: boolean;
	readonly isDirty: boolean;
}

export interface RemoteFormResetDecision {
	readonly shouldReset: boolean;
	readonly reason?: RemoteFormResetReason;
}

export type RemoteFormResetMachineEvent =
	| {
			type: 'remote.inspect';
			identityChanged: boolean;
			remoteValuesChanged: boolean;
			isDirty: boolean;
	  }
	| {type: 'remote.commit'}
	| {type: 'remote.explicitReset'}
	| {type: 'remote.clearDecision'};

interface RemoteFormResetMachineContext {
	decision: RemoteFormResetDecision;
}

const NO_RESET_DECISION: RemoteFormResetDecision = Object.freeze({shouldReset: false});

function resetDecision(reason: RemoteFormResetReason): RemoteFormResetDecision {
	return {shouldReset: true, reason};
}

export const remoteFormResetStateMachine = setup({
	types: {} as {
		context: RemoteFormResetMachineContext;
		events: RemoteFormResetMachineEvent;
	},
	actions: {
		applyInitial: assign({decision: () => resetDecision('initial')}),
		applyIdentityChange: assign({decision: () => resetDecision('identity-change')}),
		applyRemoteClean: assign({decision: () => resetDecision('remote-clean')}),
		applyCommit: assign({decision: () => resetDecision('commit')}),
		applyExplicitReset: assign({decision: () => resetDecision('explicit-reset')}),
		clearDecision: assign({decision: () => NO_RESET_DECISION}),
	},
	guards: {
		identityChanged: ({event}) => event.type === 'remote.inspect' && event.identityChanged,
		remoteChangedWhileClean: ({event}) =>
			event.type === 'remote.inspect' && event.remoteValuesChanged && !event.isDirty,
	},
}).createMachine({
	id: 'remoteFormResetPolicy',
	context: {
		decision: NO_RESET_DECISION,
	},
	initial: 'unhydrated',
	states: {
		unhydrated: {
			on: {
				'remote.inspect': {target: 'hydrated', actions: 'applyInitial'},
				'remote.commit': {target: 'hydrated', actions: 'applyCommit'},
				'remote.explicitReset': {target: 'hydrated', actions: 'applyExplicitReset'},
				'remote.clearDecision': {actions: 'clearDecision'},
			},
		},
		hydrated: {
			on: {
				'remote.inspect': [
					{guard: 'identityChanged', actions: 'applyIdentityChange'},
					{guard: 'remoteChangedWhileClean', actions: 'applyRemoteClean'},
					{actions: 'clearDecision'},
				],
				'remote.commit': {actions: 'applyCommit'},
				'remote.explicitReset': {actions: 'applyExplicitReset'},
				'remote.clearDecision': {actions: 'clearDecision'},
			},
		},
	},
});

export type RemoteFormResetMachineSnapshot = SnapshotFrom<typeof remoteFormResetStateMachine>;
export type RemoteFormResetMachineStateValue = 'unhydrated' | 'hydrated';

export function createRemoteFormResetMachineSnapshot(): RemoteFormResetMachineSnapshot {
	return initialTransition(remoteFormResetStateMachine)[0];
}

export function transitionRemoteFormResetSnapshot(
	snapshot: RemoteFormResetMachineSnapshot,
	event: RemoteFormResetMachineEvent,
): RemoteFormResetMachineSnapshot {
	return transition(remoteFormResetStateMachine, snapshot, event)[0] as RemoteFormResetMachineSnapshot;
}

export function getRemoteFormResetMachineStateValue(
	snapshot: RemoteFormResetMachineSnapshot,
): RemoteFormResetMachineStateValue {
	return snapshot.value === 'hydrated' ? 'hydrated' : 'unhydrated';
}

export function getRemoteFormResetDecision({
	hasAppliedRemoteValues,
	identityChanged,
	remoteValuesChanged,
	isDirty,
}: RemoteFormResetDecisionInput): RemoteFormResetDecision {
	let snapshot = createRemoteFormResetMachineSnapshot();
	if (hasAppliedRemoteValues) {
		snapshot = transitionRemoteFormResetSnapshot(snapshot, {type: 'remote.commit'});
	}
	return transitionRemoteFormResetSnapshot(snapshot, {
		type: 'remote.inspect',
		identityChanged,
		remoteValuesChanged,
		isDirty,
	}).context.decision;
}

type RemoteFormValueMapper<TFormValues extends FieldValues, TRemoteValues> = (
	remoteValues: TRemoteValues,
) => TFormValues;

interface UseRemoteFormResetBaseOptions<TFormValues extends FieldValues, TRemoteValues> {
	readonly form: UseFormReturn<TFormValues>;
	readonly identityKey: RemoteFormIdentityKey;
	readonly remoteValues: TRemoteValues | null | undefined;
	readonly enabled?: boolean;
	readonly isDirty?: boolean;
	readonly resetOptions?: RemoteFormResetOptions;
	readonly areRemoteValuesEqual?: (left: TRemoteValues, right: TRemoteValues) => boolean;
	readonly onApply?: (remoteValues: TRemoteValues, reason: RemoteFormResetReason) => void;
}

type UseRemoteFormResetOptions<
	TFormValues extends FieldValues,
	TRemoteValues = TFormValues,
> = UseRemoteFormResetBaseOptions<TFormValues, TRemoteValues> &
	([TRemoteValues] extends [TFormValues]
		? {readonly getFormValues?: RemoteFormValueMapper<TFormValues, TRemoteValues>}
		: {readonly getFormValues: RemoteFormValueMapper<TFormValues, TRemoteValues>});

interface UseRemoteFormResetResult<TRemoteValues> {
	readonly resetToRemoteValues: () => void;
	readonly commitRemoteValues: (remoteValues: TRemoteValues) => void;
}

function identityFormValues<TFormValues extends FieldValues>(remoteValues: TFormValues): TFormValues {
	return remoteValues;
}

function resolveFormValueMapper<TFormValues extends FieldValues, TRemoteValues>(
	getFormValues: RemoteFormValueMapper<TFormValues, TRemoteValues> | undefined,
): RemoteFormValueMapper<TFormValues, TRemoteValues> {
	return getFormValues ?? (identityFormValues as RemoteFormValueMapper<TFormValues, TRemoteValues>);
}

export function useRemoteFormReset<TFormValues extends FieldValues, TRemoteValues = TFormValues>({
	form,
	identityKey,
	remoteValues,
	enabled = true,
	isDirty,
	resetOptions,
	getFormValues,
	areRemoteValuesEqual = isEqual,
	onApply,
}: UseRemoteFormResetOptions<TFormValues, TRemoteValues>): UseRemoteFormResetResult<TRemoteValues> {
	const latestRemoteValuesRef = useRef<TRemoteValues | null | undefined>(remoteValues);
	const latestIdentityKeyRef = useRef<RemoteFormIdentityKey>(identityKey);
	const latestResetOptionsRef = useRef(resetOptions);
	const latestGetFormValuesRef = useRef(resolveFormValueMapper(getFormValues));
	const latestAreRemoteValuesEqualRef = useRef(areRemoteValuesEqual);
	const latestOnApplyRef = useRef(onApply);
	const resetMachineSnapshotRef = useRef(createRemoteFormResetMachineSnapshot());
	const lastAppliedIdentityKeyRef = useRef<RemoteFormIdentityKey>(undefined);
	const lastAppliedRemoteValuesRef = useRef<TRemoteValues | null>(null);

	const resolvedIsDirty = isDirty ?? form.formState.isDirty;

	latestRemoteValuesRef.current = remoteValues;
	latestIdentityKeyRef.current = identityKey;
	latestResetOptionsRef.current = resetOptions;
	latestGetFormValuesRef.current = resolveFormValueMapper(getFormValues);
	latestAreRemoteValuesEqualRef.current = areRemoteValuesEqual;
	latestOnApplyRef.current = onApply;

	const applyRemoteValues = useCallback(
		(values: TRemoteValues, reason: RemoteFormResetReason, shouldTransitionMachine = true) => {
			if (shouldTransitionMachine) {
				resetMachineSnapshotRef.current = transitionRemoteFormResetSnapshot(resetMachineSnapshotRef.current, {
					type: reason === 'explicit-reset' ? 'remote.explicitReset' : 'remote.commit',
				});
			}
			form.reset(latestGetFormValuesRef.current(values), latestResetOptionsRef.current);
			lastAppliedIdentityKeyRef.current = latestIdentityKeyRef.current;
			lastAppliedRemoteValuesRef.current = values;
			latestOnApplyRef.current?.(values, reason);
		},
		[form],
	);

	useLayoutEffect(() => {
		if (!enabled || remoteValues == null) return;
		const identityChanged = !Object.is(lastAppliedIdentityKeyRef.current, identityKey);
		const lastAppliedRemoteValues = lastAppliedRemoteValuesRef.current;
		const remoteValuesChanged =
			getRemoteFormResetMachineStateValue(resetMachineSnapshotRef.current) === 'unhydrated' ||
			lastAppliedRemoteValues == null ||
			!latestAreRemoteValuesEqualRef.current(lastAppliedRemoteValues, remoteValues);
		const snapshot = transitionRemoteFormResetSnapshot(resetMachineSnapshotRef.current, {
			type: 'remote.inspect',
			identityChanged,
			remoteValuesChanged,
			isDirty: resolvedIsDirty,
		});
		resetMachineSnapshotRef.current = snapshot;
		const decision = snapshot.context.decision;
		if (decision.shouldReset && decision.reason) {
			applyRemoteValues(remoteValues, decision.reason, false);
		}
	}, [applyRemoteValues, enabled, identityKey, remoteValues, resolvedIsDirty]);

	const resetToRemoteValues = useCallback(() => {
		const latestRemoteValues = latestRemoteValuesRef.current;
		if (latestRemoteValues == null) return;
		applyRemoteValues(latestRemoteValues, 'explicit-reset');
	}, [applyRemoteValues]);

	const commitRemoteValues = useCallback(
		(nextRemoteValues: TRemoteValues) => {
			applyRemoteValues(nextRemoteValues, 'commit');
		},
		[applyRemoteValues],
	);

	return {resetToRemoteValues, commitRemoteValues};
}
