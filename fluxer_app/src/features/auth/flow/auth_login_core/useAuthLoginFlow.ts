// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuthLoginStep} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import {startTransition, useMemo, useReducer} from 'react';

type MethodBackTarget = typeof AuthLoginStep.ACCOUNT | typeof AuthLoginStep.INSTANCE;

interface AuthLoginFlowState {
	showAccountSelector: boolean;
	hasCompletedInstanceStep: boolean;
	hasSelectedLoginMethod: boolean;
	showBrowserStep: boolean;
	methodBackTarget: MethodBackTarget;
	prefillIdentifier: string | null;
	error: string | null;
}

interface AuthLoginFlowSync {
	startWithAddAccount: boolean;
	relogin: boolean;
	initialIdentifier: string | null;
	initialInstanceSelected: boolean;
	shouldShowInstanceSelector: boolean;
}

export interface AuthLoginFlowInit extends AuthLoginFlowSync {
	desktopHandoff: boolean;
	hasStoredAccounts: boolean;
}

export function hasInitialIdentifier(initialIdentifier: string | null): boolean {
	if (initialIdentifier == null) {
		return false;
	}
	return initialIdentifier.length > 0;
}

function shouldShowStoredAccountSelector(init: AuthLoginFlowInit): boolean {
	if (init.startWithAddAccount) return false;
	if (init.relogin) return false;
	if (init.desktopHandoff) return false;
	if (!init.hasStoredAccounts) return false;
	return !hasInitialIdentifier(init.initialIdentifier);
}

function hasCompletedInitialInstanceStep(init: AuthLoginFlowSync): boolean {
	if (!init.shouldShowInstanceSelector) return true;
	return init.initialInstanceSelected;
}

function skipsMethodStep(sync: AuthLoginFlowSync): boolean {
	return hasInitialIdentifier(sync.initialIdentifier) && !sync.relogin;
}

function syncTargetsAccount(sync: AuthLoginFlowSync): boolean {
	return hasInitialIdentifier(sync.initialIdentifier) || sync.relogin;
}

function initialAuthLoginFlowState(init: AuthLoginFlowInit): AuthLoginFlowState {
	return {
		showAccountSelector: shouldShowStoredAccountSelector(init),
		hasCompletedInstanceStep: hasCompletedInitialInstanceStep(init),
		hasSelectedLoginMethod: skipsMethodStep(init),
		showBrowserStep: false,
		methodBackTarget: AuthLoginStep.INSTANCE,
		prefillIdentifier: init.initialIdentifier,
		error: null,
	};
}

type AuthLoginFlowAction =
	| {type: 'syncFromProps'; sync: AuthLoginFlowSync}
	| {type: 'showMethodsForAccount'; prefillIdentifier: string | null; hasCompletedInstanceStep: boolean}
	| {type: 'addAnotherAccount'}
	| {type: 'backToAccountList'}
	| {type: 'changeInstance'}
	| {type: 'continueFromInstance'}
	| {type: 'selectEmailMethod'}
	| {type: 'enterBrowserStep'}
	| {type: 'backToMethodStep'}
	| {type: 'setError'; error: string | null};

function reduceAuthLoginFlow(state: AuthLoginFlowState, action: AuthLoginFlowAction): AuthLoginFlowState {
	switch (action.type) {
		case 'syncFromProps': {
			const {sync} = action;
			const hasCompletedInstanceStep = hasCompletedInitialInstanceStep(sync);
			const base: AuthLoginFlowState = {
				...state,
				prefillIdentifier: sync.initialIdentifier,
				showBrowserStep: false,
				hasSelectedLoginMethod: skipsMethodStep(sync),
			};
			if (syncTargetsAccount(sync)) {
				return {
					...base,
					showAccountSelector: false,
					hasCompletedInstanceStep,
					methodBackTarget: AuthLoginStep.INSTANCE,
				};
			}
			if (sync.startWithAddAccount) {
				return {
					...base,
					showAccountSelector: false,
					hasCompletedInstanceStep: !sync.shouldShowInstanceSelector,
					methodBackTarget: AuthLoginStep.INSTANCE,
				};
			}
			if (!sync.shouldShowInstanceSelector) {
				return {...base, hasCompletedInstanceStep: true};
			}
			return base;
		}
		case 'showMethodsForAccount':
			return {
				showAccountSelector: false,
				hasCompletedInstanceStep: action.hasCompletedInstanceStep,
				hasSelectedLoginMethod: false,
				showBrowserStep: false,
				methodBackTarget: AuthLoginStep.ACCOUNT,
				prefillIdentifier: action.prefillIdentifier,
				error: null,
			};
		case 'addAnotherAccount':
			return {
				showAccountSelector: false,
				hasCompletedInstanceStep: false,
				hasSelectedLoginMethod: false,
				showBrowserStep: false,
				methodBackTarget: AuthLoginStep.ACCOUNT,
				prefillIdentifier: null,
				error: null,
			};
		case 'backToAccountList':
			return {
				showAccountSelector: true,
				hasCompletedInstanceStep: true,
				hasSelectedLoginMethod: false,
				showBrowserStep: false,
				methodBackTarget: AuthLoginStep.INSTANCE,
				prefillIdentifier: null,
				error: null,
			};
		case 'changeInstance':
			return {
				...state,
				hasCompletedInstanceStep: false,
				hasSelectedLoginMethod: false,
				showBrowserStep: false,
				error: null,
			};
		case 'continueFromInstance':
			return {...state, hasCompletedInstanceStep: true, showBrowserStep: false, error: null};
		case 'selectEmailMethod':
			return {...state, hasSelectedLoginMethod: true, showBrowserStep: false, error: null};
		case 'enterBrowserStep':
			return {...state, hasSelectedLoginMethod: true, showBrowserStep: true, error: null};
		case 'backToMethodStep':
			return {...state, showBrowserStep: false, hasSelectedLoginMethod: false, error: null};
		case 'setError':
			return {...state, error: action.error};
	}
}

export interface AuthLoginFlow extends AuthLoginFlowState {
	syncFromProps: (sync: AuthLoginFlowSync) => void;
	showMethodsForAccount: (prefillIdentifier: string | null, hasCompletedInstanceStep: boolean) => void;
	addAnotherAccount: () => void;
	backToAccountList: () => void;
	changeInstance: () => void;
	continueFromInstance: () => void;
	selectEmailMethod: () => void;
	enterBrowserStep: () => void;
	backToMethodStep: () => void;
	setError: (error: string | null) => void;
}

export function useAuthLoginFlow(init: AuthLoginFlowInit): AuthLoginFlow {
	const [state, dispatch] = useReducer(reduceAuthLoginFlow, init, initialAuthLoginFlowState);
	const actions = useMemo(
		() => ({
			syncFromProps: (sync: AuthLoginFlowSync) => dispatch({type: 'syncFromProps', sync}),
			showMethodsForAccount: (prefillIdentifier: string | null, hasCompletedInstanceStep: boolean) =>
				dispatch({type: 'showMethodsForAccount', prefillIdentifier, hasCompletedInstanceStep}),
			addAnotherAccount: () => startTransition(() => dispatch({type: 'addAnotherAccount'})),
			backToAccountList: () => startTransition(() => dispatch({type: 'backToAccountList'})),
			changeInstance: () => startTransition(() => dispatch({type: 'changeInstance'})),
			continueFromInstance: () => startTransition(() => dispatch({type: 'continueFromInstance'})),
			selectEmailMethod: () => startTransition(() => dispatch({type: 'selectEmailMethod'})),
			enterBrowserStep: () => startTransition(() => dispatch({type: 'enterBrowserStep'})),
			backToMethodStep: () => startTransition(() => dispatch({type: 'backToMethodStep'})),
			setError: (error: string | null) => dispatch({type: 'setError', error}),
		}),
		[],
	);
	return {...state, ...actions};
}
