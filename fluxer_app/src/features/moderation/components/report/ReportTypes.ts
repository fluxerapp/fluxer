// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportFlowWalk} from '@app/features/moderation/components/report_flow/ReportFlowWalk';
import type {ReportFlowResponse} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';

export type FlowStep = 'selection' | 'email' | 'verification' | 'reason' | 'details' | 'complete';
export type ReportType = 'message' | 'user' | 'guild';
export type ReportFlowStatus = 'loading' | 'loaded' | 'unavailable';

export const INITIAL_FORM_VALUES = {
	reporterFullName: '',
	reporterCountry: '',
	messageLink: '',
	messageUserTag: '',
	userId: '',
	userTag: '',
	guildId: '',
	inviteCode: '',
	additionalInfo: '',
};

export type FormValues = typeof INITIAL_FORM_VALUES;
export type ReportField = keyof FormValues | 'goodFaithConfirmed';

export interface State {
	selectedType: ReportType | null;
	flowStep: FlowStep;
	email: string;
	verificationCode: string;
	ticket: string | null;
	formValues: FormValues;
	prefillOption: string | null;
	goodFaithConfirmed: boolean;
	flow: ReportFlowResponse | null;
	flowStatus: ReportFlowStatus;
	walk: ReportFlowWalk | null;
	answersRejected: boolean;
	isSendingCode: boolean;
	isVerifying: boolean;
	isSubmitting: boolean;
	errorMessage: string | null;
	errorIsRateLimit: boolean;
	successReportId: string | null;
	resendCooldownSeconds: number;
	fieldErrors: Partial<Record<ReportField, string>>;
}

export type Action =
	| {
			type: 'RESET_ALL';
	  }
	| {
			type: 'SELECT_TYPE';
			reportType: ReportType;
	  }
	| {
			type: 'PREFILL';
			reportType: ReportType;
			values: Partial<FormValues>;
			option: string | null;
	  }
	| {
			type: 'GO_TO_SELECTION';
	  }
	| {
			type: 'GO_TO_EMAIL';
	  }
	| {
			type: 'GO_TO_VERIFICATION';
	  }
	| {
			type: 'GO_TO_REASON';
	  }
	| {
			type: 'GO_TO_DETAILS';
	  }
	| {
			type: 'FLOW_REQUESTED';
	  }
	| {
			type: 'FLOW_LOADED';
			flow: ReportFlowResponse;
	  }
	| {
			type: 'FLOW_UNAVAILABLE';
			reportType: ReportType;
	  }
	| {
			type: 'WALK_CHANGED';
			walk: ReportFlowWalk;
	  }
	| {
			type: 'ANSWERS_REJECTED';
			message: string;
	  }
	| {
			type: 'SET_ERROR';
			message: string | null;
			rateLimit?: boolean;
	  }
	| {
			type: 'SET_EMAIL';
			email: string;
	  }
	| {
			type: 'SET_VERIFICATION_CODE';
			code: string;
	  }
	| {
			type: 'SET_TICKET';
			ticket: string | null;
	  }
	| {
			type: 'SET_FORM_FIELD';
			field: keyof FormValues;
			value: string;
	  }
	| {
			type: 'SET_GOOD_FAITH_CONFIRMED';
			value: boolean;
	  }
	| {
			type: 'SENDING_CODE';
			value: boolean;
	  }
	| {
			type: 'VERIFYING';
			value: boolean;
	  }
	| {
			type: 'SUBMITTING';
			value: boolean;
	  }
	| {
			type: 'SUBMIT_SUCCESS';
			reportId: string;
	  }
	| {
			type: 'START_RESEND_COOLDOWN';
			seconds: number;
	  }
	| {
			type: 'TICK_RESEND_COOLDOWN';
	  }
	| {
			type: 'SET_FIELD_ERRORS';
			errors: Partial<Record<ReportField, string>>;
	  }
	| {
			type: 'CLEAR_FIELD_ERRORS';
	  }
	| {
			type: 'CLEAR_FIELD_ERROR';
			field: ReportField;
	  };
