// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModalInPortal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import Accounts from '@app/features/auth/state/Accounts';
import {
	SOMETHING_WENT_WRONG_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {I18n} from '@lingui/core';

export type UserSettingsMutation = () => Promise<void>;

export interface UserSettingsMutationController {
	readonly settle: (mutation: UserSettingsMutation) => void;
}

interface UserSettingsMutationPresentationOwnerDependencies {
	readonly accountKey: string;
	readonly i18n: I18n;
	readonly portalHost: HTMLElement;
}

const logger = new Logger('UserSettingsMutationPresentation');

export class UserSettingsMutationPresentationOwner {
	private disposed = false;

	public constructor(private readonly dependencies: UserSettingsMutationPresentationOwnerDependencies) {}

	public settle(mutation: UserSettingsMutation): void {
		if (this.disposed) {
			return;
		}
		this.createOperation(mutation).catch((error: unknown) => {
			this.presentFailure(error);
		});
	}

	public dispose(): void {
		this.disposed = true;
	}

	private createOperation(mutation: UserSettingsMutation): Promise<void> {
		try {
			return mutation();
		} catch (error) {
			return Promise.reject(error);
		}
	}

	private presentFailure(mutationError: unknown): void {
		if (this.disposed || Accounts.currentAccountKey !== this.dependencies.accountKey) {
			return;
		}
		try {
			showGenericErrorModalInPortal({
				title: () => this.dependencies.i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
				message: () => this.dependencies.i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				portalHost: this.dependencies.portalHost,
			});
		} catch (presentationError) {
			logger.error(
				'User settings mutation and failure presentation failed',
				new AggregateError([mutationError, presentationError]),
			);
		}
	}
}
