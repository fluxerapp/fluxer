// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuthLoginStep} from '@app/features/auth/flow/auth_login_core/AuthLoginStepTypes';
import {startTransition, useMemo, useReducer} from 'react';

export type CredentialsBackTarget = typeof AuthLoginStep.ACCOUNT | typeof AuthLoginStep.METHOD;

export type MethodBackTarget = typeof AuthLoginStep.ACCOUNT | typeof AuthLoginStep.INSTANCE;

export interface AuthLoginFlowState {
	showAccountSelector: boolean;
	hasCompletedInstanceStep: boolean;
	hasSelectedLoginMethod: boolean;
	showBrowserStep: boolean;
	credentialsBackTarget: CredentialsBackTarget;
	methodBackTarget: MethodBackTarget;
	prefillIdentifier: string | null;
	error: string | null;
}

export interface AuthLoginFlowSync {
	startWithAddAccount: boolean;
	forceCredentials: boolean;
	initialIdentifier: string | null;
	initialInstanceSelected: boolean;
	shouldShowInstanceSelector: boolean;
}

export interface AuthLoginFlowInit extends AuthLoginFlowSync {
	desktopHandoff: boolean;
	hasStoredAccounts: boolean;
}

function hasInitialIdentifier(initialIdentifier: string | null): boolean {
	if (initialIdentifier == null) {
		return false;
	}
	return initialIdentifier.length > 0;
}

function shouldShowStoredAccountSelector(init: AuthLoginFlowInit): boolean {
	if (init.startWithAddAccount) return false;
	if (init.forceCredentials) return false;
	if (init.desktopHandoff) return false;
	if (!init.hasStoredAccounts) return false;
	return !hasInitialIdentifier(init.initialIdentifier);
}

function hasCompletedInitialInstanceStep(init: AuthLoginFlowSync): boolean {
	if (!init.shouldShowInstanceSelector) return true;
	return init.initialInstanceSelected;
}

function syncRequiresCredentials(sync: AuthLoginFlowSync): boolean {
	return hasInitialIdentifier(sync.initialIdentifier) || sync.forceCredentials;
}

function initialAuthLoginFlowState(init: AuthLoginFlowInit): AuthLoginFlowState {
	return {
		showAccountSelector: shouldShowStoredAccountSelector(init),
		hasCompletedInstanceStep: hasCompletedInitialInstanceStep(init),
		hasSelectedLoginMethod: hasInitialIdentifier(init.initialIdentifier),
		showBrowserStep: false,
		credentialsBackTarget: AuthLoginStep.METHOD,
		methodBackTarget: AuthLoginStep.INSTANCE,
		prefillIdentifier: init.initialIdentifier,
		error: null,
	};
}

export type AuthLoginFlowAction =
	| {type: 'syncFromProps'; sync: AuthLoginFlowSync}
	| {
			type: 'showFormForAccount';
			prefillIdentifier: string | null;
			message: string | null;
			hasCompletedInstanceStep: boolean;
	  }
	| {type: 'addAnotherAccount'}
	| {type: 'backToAccountList'}
	| {type: 'changeInstance'}
	| {type: 'continueFromInstance'}
	| {type: 'selectEmailMethod'}
	| {type: 'enterBrowserStep'}
	| {type: 'backFromCredentials'; allowReturnToAccountList: boolean}
	| {type: 'backFromBrowser'}
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
				hasSelectedLoginMethod: hasInitialIdentifier(sync.initialIdentifier),
			};
			if (syncRequiresCredentials(sync)) {
				return {
					...base,
					showAccountSelector: false,
					hasCompletedInstanceStep,
					credentialsBackTarget: AuthLoginStep.METHOD,
					methodBackTarget: AuthLoginStep.INSTANCE,
				};
			}
			if (sync.startWithAddAccount) {
				return {
					...base,
					showAccountSelector: false,
					hasCompletedInstanceStep: !sync.shouldShowInstanceSelector,
					credentialsBackTarget: AuthLoginStep.METHOD,
					methodBackTarget: AuthLoginStep.INSTANCE,
				};
			}
			if (!sync.shouldShowInstanceSelector) {
				return {...base, hasCompletedInstanceStep: true};
			}
			return base;
		}
		case 'showFormForAccount':
			return {
				showAccountSelector: false,
				hasCompletedInstanceStep: action.hasCompletedInstanceStep,
				hasSelectedLoginMethod: true,
				showBrowserStep: false,
				credentialsBackTarget: AuthLoginStep.ACCOUNT,
				methodBackTarget: AuthLoginStep.INSTANCE,
				prefillIdentifier: action.prefillIdentifier,
				error: action.message,
			};
		case 'addAnotherAccount':
			return {
				showAccountSelector: false,
				hasCompletedInstanceStep: false,
				hasSelectedLoginMethod: false,
				showBrowserStep: false,
				credentialsBackTarget: AuthLoginStep.METHOD,
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
				credentialsBackTarget: AuthLoginStep.METHOD,
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
				credentialsBackTarget: AuthLoginStep.METHOD,
				error: null,
			};
		case 'continueFromInstance':
			return {...state, hasCompletedInstanceStep: true, showBrowserStep: false, error: null};
		case 'selectEmailMethod':
			return {
				...state,
				hasSelectedLoginMethod: true,
				showBrowserStep: false,
				credentialsBackTarget: AuthLoginStep.METHOD,
				error: null,
			};
		case 'enterBrowserStep':
			return {...state, hasSelectedLoginMethod: true, showBrowserStep: true, error: null};
		case 'backFromCredentials': {
			if (state.credentialsBackTarget === AuthLoginStep.ACCOUNT && action.allowReturnToAccountList) {
				return {
					...state,
					showBrowserStep: false,
					prefillIdentifier: null,
					showAccountSelector: true,
					hasCompletedInstanceStep: true,
					hasSelectedLoginMethod: false,
					methodBackTarget: AuthLoginStep.INSTANCE,
					error: null,
				};
			}
			return {...state, showBrowserStep: false, hasSelectedLoginMethod: false, error: null};
		}
		case 'backFromBrowser':
			return {...state, showBrowserStep: false, hasSelectedLoginMethod: false, error: null};
		case 'setError':
			return {...state, error: action.error};
	}
}

export interface AuthLoginFlow extends AuthLoginFlowState {
	syncFromProps: (sync: AuthLoginFlowSync) => void;
	showFormForAccount: (
		prefillIdentifier: string | null,
		message: string | null,
		hasCompletedInstanceStep?: boolean,
	) => void;
	addAnotherAccount: () => void;
	backToAccountList: () => void;
	changeInstance: () => void;
	continueFromInstance: () => void;
	selectEmailMethod: () => void;
	enterBrowserStep: () => void;
	backFromCredentials: (allowReturnToAccountList: boolean) => void;
	backFromBrowser: () => void;
	setError: (error: string | null) => void;
}

export function useAuthLoginFlow(init: AuthLoginFlowInit): AuthLoginFlow {
	const [state, dispatch] = useReducer(reduceAuthLoginFlow, init, initialAuthLoginFlowState);
	const actions = useMemo(
		() => ({
			syncFromProps: (sync: AuthLoginFlowSync) => dispatch({type: 'syncFromProps', sync}),
			showFormForAccount: (prefillIdentifier: string | null, message: string | null, hasCompletedInstanceStep = true) =>
				dispatch({type: 'showFormForAccount', prefillIdentifier, message, hasCompletedInstanceStep}),
			addAnotherAccount: () => startTransition(() => dispatch({type: 'addAnotherAccount'})),
			backToAccountList: () => startTransition(() => dispatch({type: 'backToAccountList'})),
			changeInstance: () => startTransition(() => dispatch({type: 'changeInstance'})),
			continueFromInstance: () => startTransition(() => dispatch({type: 'continueFromInstance'})),
			selectEmailMethod: () => startTransition(() => dispatch({type: 'selectEmailMethod'})),
			enterBrowserStep: () => startTransition(() => dispatch({type: 'enterBrowserStep'})),
			backFromCredentials: (allowReturnToAccountList: boolean) =>
				startTransition(() => dispatch({type: 'backFromCredentials', allowReturnToAccountList})),
			backFromBrowser: () => startTransition(() => dispatch({type: 'backFromBrowser'})),
			setError: (error: string | null) => dispatch({type: 'setError', error}),
		}),
		[],
	);
	return {...state, ...actions};
}
