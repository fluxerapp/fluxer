// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {createGuildID} from '../BrandedTypes';
import {Config} from '../Config';
import type {IGuildMemberRepository} from '../guild/repositories/IGuildMemberRepository';
import type {User} from '../models/User';
import {accountPolicyContactHasCapability} from '../risk/AccountPolicyService';
import {checkHasActivePaidPremium} from '../user/UserHelpers';
import {isFluxerFlutterAndroidClient, isFluxerFlutterClient, isFluxerFlutterIosClient} from '../utils/UserAgentUtils';

const FLUTTER_CLIENT_ALLOWED_GUILD_ID = createGuildID(1489322182823577203n);
const ANDROID_FLUTTER_OPEN_ACCESS_AT_MS = Date.parse('2026-06-15T21:00:00.000Z');

export type FlutterClientGateMemberRepository = Pick<IGuildMemberRepository, 'getMember'>;

export async function assertFlutterClientLoginAllowed(
	request: Request,
	user: User,
	memberRepository: FlutterClientGateMemberRepository,
): Promise<void> {
	if (!isFluxerFlutterClient(request)) return;
	if (Config.instance.selfHosted) return;
	if (accountPolicyContactHasCapability(user.email, 'client_gate_exempt')) return;
	if (isFluxerFlutterAndroidClient(request) && isAndroidOpenAccessActive()) return;
	if (isFluxerFlutterIosClient(request) && checkHasActivePaidPremium(user)) return;
	const member = await memberRepository.getMember(FLUTTER_CLIENT_ALLOWED_GUILD_ID, user.id);
	if (member) return;
	throw InputValidationError.fromCodes([
		{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
		{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
	]);
}

export function assertFlutterClientRegistrationAllowed(request: Request, email: string | null | undefined): void {
	if (!isFluxerFlutterClient(request)) return;
	if (Config.instance.selfHosted) return;
	if (accountPolicyContactHasCapability(email, 'client_gate_exempt')) return;
	if (isFluxerFlutterAndroidClient(request) && isAndroidOpenAccessActive()) return;
	throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
}

function isAndroidOpenAccessActive(): boolean {
	return Date.now() >= ANDROID_FLUTTER_OPEN_ACCESS_AT_MS;
}
