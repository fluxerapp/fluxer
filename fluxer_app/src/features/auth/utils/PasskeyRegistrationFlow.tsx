// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {PasskeyNameModal} from '@app/features/auth/components/modals/PasskeyNameModal';
import {describePasskeyError} from '@app/features/auth/utils/PasskeyErrorMessages';
import {createPasskeyRegistration, preparePasskeyRegistration} from '@app/features/auth/utils/PasskeyRegistration';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isAbortError} from '@app/features/platform/utils/PlatformHelpers';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as FormUtils from '@app/lib/forms';
import type {I18n} from '@lingui/core';

export async function startPasskeyRegistration(i18n: I18n): Promise<void> {
	let options: Awaited<ReturnType<typeof preparePasskeyRegistration>>;
	try {
		options = await preparePasskeyRegistration();
	} catch (error) {
		if (isAbortError(error)) return;
		const message = describePasskeyError(i18n, error, null);
		if (message === null) {
			FormUtils.pushApiErrorModal(i18n, error);
			return;
		}
		ModalCommands.push(
			modal(() => (
				<GenericErrorModal
					title={i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR)}
					message={message}
					data-flx="auth.passkey-registration-flow.generic-error-modal"
				/>
			)),
		);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<PasskeyNameModal
				rpId={options.rp.id ?? null}
				onSubmit={createPasskeyRegistration(options)}
				data-flx="auth.passkey-registration-flow.passkey-name-modal"
			/>
		)),
	);
}
