// SPDX-License-Identifier: AGPL-3.0-or-later

import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import styles from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetail.module.css';
import {SectionCard} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailSectionCard';
import {
	BOT_TOKEN_DESCRIPTOR,
	CLIENT_SECRET_DESCRIPTOR,
	REGENERATE_DESCRIPTOR,
	SECRETS_TOKENS_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

const KEEP_THESE_SAFE_REGENERATING_WILL_BREAK_EXISTING_INTEGRATIONS_DESCRIPTOR = msg({
	message: 'Keep these safe. Regenerating will break existing integrations.',
	comment: 'Description text in the secrets section.',
});

interface SecretsSectionProps {
	clientSecret: string | null;
	botToken: string | null;
	onRegenerateClientSecret: () => void;
	onRegenerateBotToken: () => void;
	isRotatingClient: boolean;
	isRotatingBot: boolean;
	hasBot: boolean;
	clientSecretInputId: string;
	botTokenInputId: string;
}

export const SecretsSection: React.FC<SecretsSectionProps> = ({
	clientSecret,
	botToken,
	onRegenerateClientSecret,
	onRegenerateBotToken,
	isRotatingClient,
	isRotatingBot,
	hasBot,
	clientSecretInputId,
	botTokenInputId,
}) => {
	const {i18n} = useLingui();
	return (
		<SectionCard
			title={i18n._(SECRETS_TOKENS_DESCRIPTOR)}
			subtitle={i18n._(KEEP_THESE_SAFE_REGENERATING_WILL_BREAK_EXISTING_INTEGRATIONS_DESCRIPTOR)}
			data-flx="user.applications-tab.application-detail.secrets-section.section-card"
		>
			<div
				className={styles.fieldStack}
				data-flx="user.applications-tab.application-detail.secrets-section.field-stack"
			>
				<div
					className={styles.secretRow}
					data-flx="user.applications-tab.application-detail.secrets-section.secret-row"
				>
					<Input
						id={clientSecretInputId}
						label={i18n._(CLIENT_SECRET_DESCRIPTOR)}
						type="text"
						value={clientSecret ?? ''}
						readOnly
						placeholder={clientSecret ? '•'.repeat(64) : '•'.repeat(64)}
						data-flx="user.applications-tab.application-detail.secrets-section.input.text"
					/>
					<div
						className={styles.secretActions}
						data-flx="user.applications-tab.application-detail.secrets-section.secret-actions"
					>
						<Button
							variant="primary"
							compact
							submitting={isRotatingClient}
							onClick={onRegenerateClientSecret}
							data-flx="user.applications-tab.application-detail.secrets-section.button.regenerate-client-secret"
						>
							{i18n._(REGENERATE_DESCRIPTOR)}
						</Button>
					</div>
				</div>
				{hasBot && (
					<div
						className={styles.secretRow}
						data-flx="user.applications-tab.application-detail.secrets-section.secret-row--2"
					>
						<Input
							id={botTokenInputId}
							label={i18n._(BOT_TOKEN_DESCRIPTOR)}
							type="text"
							value={botToken ?? ''}
							readOnly
							placeholder={botToken ? '•'.repeat(64) : '•'.repeat(64)}
							data-flx="user.applications-tab.application-detail.secrets-section.input.text--2"
						/>
						<div
							className={styles.secretActions}
							data-flx="user.applications-tab.application-detail.secrets-section.secret-actions--2"
						>
							<Button
								variant="primary"
								compact
								submitting={isRotatingBot}
								onClick={onRegenerateBotToken}
								data-flx="user.applications-tab.application-detail.secrets-section.button.regenerate-bot-token"
							>
								{i18n._(REGENERATE_DESCRIPTOR)}
							</Button>
						</div>
					</div>
				)}
			</div>
		</SectionCard>
	);
};
