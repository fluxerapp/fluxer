// SPDX-License-Identifier: AGPL-3.0-or-later

import i18nGlobal from '@app/app/I18n';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {openRecoveryKitModal} from '@app/features/auth/components/modals/RecoveryKitModal';
import type {SudoVerificationPayload} from '@app/features/auth/types/AuthSudoTypes';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import RecoveryKitStatus from '@app/features/user/state/RecoveryKitStatus';
import Users from '@app/features/user/state/Users';
import type {
	RecoveryKitCreateResponse,
	RecoveryKitStatusResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {msg} from '@lingui/core/macro';

const logger = new Logger('RecoveryKit');

const RECOVERY_KIT_CREATE_FAILED_DESCRIPTOR = msg({
	message: "Couldn't create your recovery kit. You can create one later in your account settings.",
	comment: 'Toast shown when the recovery kit cannot be created right after sign-up or claiming an account.',
});

export function rememberRecoveryKit(userId: string, createdAt: string): void {
	RecoveryKitStatus.setStatus(userId, {has_recovery_kit: true, created_at: createdAt});
}

async function createRecoveryKit(
	userId: string | null,
	proof: SudoVerificationPayload,
): Promise<RecoveryKitCreateResponse> {
	try {
		const response = await http.post<RecoveryKitCreateResponse>(Endpoints.USER_RECOVERY_KIT, {body: proof});
		if (userId) {
			rememberRecoveryKit(userId, response.body.created_at);
		}
		logger.info('Created a recovery kit');
		return response.body;
	} catch (error) {
		logger.error('Failed to create recovery kit', error);
		throw error;
	}
}

export async function fetchRecoveryKitStatus(): Promise<RecoveryKitStatusResponse> {
	const userId = Users.currentUserId;
	try {
		const response = await http.get<RecoveryKitStatusResponse>(Endpoints.USER_RECOVERY_KIT);
		if (userId) {
			RecoveryKitStatus.setStatus(userId, response.body);
		}
		return response.body;
	} catch (error) {
		logger.error('Failed to fetch recovery kit status', error);
		throw error;
	}
}

async function currentUserHasRecoveryKit(userId: string | null): Promise<boolean> {
	const known = userId ? RecoveryKitStatus.get(userId) : undefined;
	if (known) return known.hasRecoveryKit;
	try {
		return (await fetchRecoveryKitStatus()).has_recovery_kit;
	} catch {
		return true;
	}
}

export async function createAndShowRecoveryKit(): Promise<void> {
	const userId = Users.currentUserId;
	const replacing = await currentUserHasRecoveryKit(userId);
	const kit = await createRecoveryKit(userId, {});
	openRecoveryKitModal({
		recoveryKey: kit.recovery_key,
		createdAt: kit.created_at,
		reason: replacing ? 'replaced' : 'created',
	});
}

export async function createRecoveryKitWithPasswordAndOpen({
	userId,
	password,
	username,
	discriminator,
}: {
	userId: string;
	password: string;
	username?: string;
	discriminator?: string;
}): Promise<boolean> {
	try {
		const kit = await createRecoveryKit(userId, {password});
		openRecoveryKitModal({recoveryKey: kit.recovery_key, createdAt: kit.created_at, username, discriminator});
		return true;
	} catch {
		ToastCommands.error(i18nGlobal._(RECOVERY_KIT_CREATE_FAILED_DESCRIPTOR));
		return false;
	}
}

export async function replaceRecoveryKitAfterPasswordChange(newPassword: string): Promise<void> {
	const userId = Users.currentUserId;
	if (!userId) return;
	RecoveryKitStatus.setStatus(userId, {has_recovery_kit: false, created_at: null});
	try {
		const kit = Users.getCurrentUser()?.mfaEnabled
			? await createRecoveryKit(userId, {})
			: await createRecoveryKit(userId, {password: newPassword});
		openRecoveryKitModal({recoveryKey: kit.recovery_key, createdAt: kit.created_at, reason: 'replaced'});
	} catch {
		ToastCommands.error(i18nGlobal._(RECOVERY_KIT_CREATE_FAILED_DESCRIPTOR));
	}
}
