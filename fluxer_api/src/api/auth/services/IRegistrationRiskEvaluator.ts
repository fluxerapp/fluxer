// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RecommendedAction, RiskAssessment, RiskLevel} from '@app/api/risk/RiskTypes';

export interface RegistrationRiskParams {
	email: string | null;
	clientIp: string;
	locale: string | null;
	timezone: string | null;
	userAgent: string | null;
	username?: string | null;
	globalName?: string | null;
	usernameIsUserChosen?: boolean;
	isUnclaimed?: boolean;
}

export interface RegistrationRiskResult {
	assessment: RiskAssessment;
	level: RiskLevel;
	recommendedAction: RecommendedAction;
}

export interface IRegistrationRiskEvaluator {
	evaluate(params: RegistrationRiskParams): Promise<RegistrationRiskResult>;
}
