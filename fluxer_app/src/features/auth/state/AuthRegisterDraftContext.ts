// SPDX-License-Identifier: AGPL-3.0-or-later

import React, {useCallback, useContext, useMemo, useRef} from 'react';

export interface AuthRegisterFormDraft {
	formValues: Record<string, string>;
	selectedMonth: string;
	selectedDay: string;
	selectedYear: string;
	consent: boolean;
}

export const EMPTY_AUTH_REGISTER_FORM_DRAFT: AuthRegisterFormDraft = {
	formValues: {},
	selectedMonth: '',
	selectedDay: '',
	selectedYear: '',
	consent: false,
};

interface AuthRegisterDraftContextType {
	getRegisterFormDraft: (draftKey: string) => AuthRegisterFormDraft | undefined;
	setRegisterFormDraft: (draftKey: string, draft: AuthRegisterFormDraft) => void;
	clearRegisterFormDraft: (draftKey: string) => void;
}

export const AuthRegisterDraftContext = React.createContext<AuthRegisterDraftContextType | null>(null);

export function useAuthRegisterDraftContext(): AuthRegisterDraftContextType {
	const context = useContext(AuthRegisterDraftContext);
	if (!context) {
		throw new Error('useAuthRegisterDraftContext must be used within AuthRegisterDraftContext.Provider');
	}
	return context;
}

export function useAuthRegisterDraft(): AuthRegisterDraftContextType {
	const draftsRef = useRef<Map<string, AuthRegisterFormDraft>>(new Map());
	const getRegisterFormDraft = useCallback((draftKey: string): AuthRegisterFormDraft | undefined => {
		const draft = draftsRef.current.get(draftKey);
		if (!draft) {
			return undefined;
		}
		return {...draft, formValues: {...draft.formValues}};
	}, []);
	const setRegisterFormDraft = useCallback((draftKey: string, draft: AuthRegisterFormDraft) => {
		draftsRef.current.set(draftKey, {...draft, formValues: {...draft.formValues}});
	}, []);
	const clearRegisterFormDraft = useCallback((draftKey: string) => {
		draftsRef.current.delete(draftKey);
	}, []);
	return useMemo(
		() => ({getRegisterFormDraft, setRegisterFormDraft, clearRegisterFormDraft}),
		[clearRegisterFormDraft, getRegisterFormDraft, setRegisterFormDraft],
	);
}
