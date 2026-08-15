// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXAMPLE_CALLBACK_URL} from '@app/features/app/config/I18nDisplayConstants';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import styles from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetail.module.css';
import {SectionCard} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailSectionCard';
import type {ApplicationDetailForm} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailTypes';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import type React from 'react';

const APPLICATION_INFORMATION_DESCRIPTOR = msg({
	message: 'Application information',
	comment: 'Short label in the application info section. Keep it concise.',
});
const BASIC_SETTINGS_AND_ALLOWED_REDIRECT_URIS_DESCRIPTOR = msg({
	message: 'Basic settings and allowed redirect URIs.',
	comment: 'Description text in the application info section.',
});
const APPLICATION_NAME_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'Application name is required',
	comment: 'Label in the application info section.',
});
const APPLICATION_NAME_DESCRIPTOR = msg({
	message: 'Application name',
	comment: 'Short label in the application info section. Keep it concise.',
});
const MY_APPLICATION_DESCRIPTOR = msg({
	message: 'My application',
	comment: 'Short label in the application info section. Keep it concise.',
});
const PUBLIC_BOT_DESCRIPTOR = msg({
	message: 'Public bot',
	comment: 'Short label in the application info section. Keep it concise.',
});
const ALLOW_ANYONE_TO_INVITE_THIS_BOT_TO_THEIR_DESCRIPTOR = msg({
	message: 'Allow anyone to invite this bot to their communities.',
	comment: 'Description text in the application info section.',
});
const REQUIRE_OAUTH2_CODE_GRANT_DESCRIPTOR = msg({
	message: 'Require OAuth2 code grant',
	comment: 'Label in the application info section. Keep the tone plain and specific.',
});
const WHEN_ENABLED_INVITING_THIS_BOT_REQUIRES_A_REDIRECT_DESCRIPTOR = msg({
	message: 'Requires a redirect URI and an authorization code when inviting this bot.',
	comment: 'Description text in the application info section. Keep the tone plain and specific.',
});
const REDIRECT_URIS_DESCRIPTOR = msg({
	message: 'Redirect URIs',
	comment: 'Short label in the application info section. Keep it concise.',
});
const DELETE_REDIRECT_URI_DESCRIPTOR = msg({
	message: 'Delete redirect URI',
	comment:
		'Button or menu action label in the application info section. Keep it concise. Keep the tone plain and specific.',
});
const ADD_REDIRECT_DESCRIPTOR = msg({
	message: 'Add redirect',
	comment: 'Button or menu action label in the application info section. Keep it concise.',
});

interface ApplicationInfoSectionProps {
	form: ApplicationDetailForm;
	redirectInputs: Array<string>;
	onAddRedirect: () => void;
	onRemoveRedirect: (index: number) => void;
	onUpdateRedirect: (index: number, value: string) => void;
}

export const ApplicationInfoSection: React.FC<ApplicationInfoSectionProps> = ({
	form,
	redirectInputs,
	onAddRedirect,
	onRemoveRedirect,
	onUpdateRedirect,
}) => {
	const {i18n} = useLingui();
	const redirectList = redirectInputs ?? [];
	const getRedirectError = (index: number) =>
		form.getFieldState(`redirectUriInputs.${index}` as `redirectUriInputs.${number}`, form.formState).error?.message;
	return (
		<SectionCard
			title={i18n._(APPLICATION_INFORMATION_DESCRIPTOR)}
			subtitle={i18n._(BASIC_SETTINGS_AND_ALLOWED_REDIRECT_URIS_DESCRIPTOR)}
			data-flx="user.applications-tab.application-detail.application-info-section.section-card"
		>
			<div
				className={styles.fieldStack}
				data-flx="user.applications-tab.application-detail.application-info-section.field-stack"
			>
				<Input
					data-flx="user.applications-tab.application-detail.application-info-section.input"
					{...form.register('name', {required: i18n._(APPLICATION_NAME_IS_REQUIRED_DESCRIPTOR)})}
					label={i18n._(APPLICATION_NAME_DESCRIPTOR)}
					value={form.watch('name')}
					placeholder={i18n._(MY_APPLICATION_DESCRIPTOR)}
					maxLength={100}
					error={form.formState.errors.name?.message}
				/>
				<Switch
					label={i18n._(PUBLIC_BOT_DESCRIPTOR)}
					description={i18n._(ALLOW_ANYONE_TO_INVITE_THIS_BOT_TO_THEIR_DESCRIPTOR)}
					value={form.watch('botPublic')}
					onChange={(checked) => form.setValue('botPublic', checked, {shouldDirty: true})}
					data-flx="user.applications-tab.application-detail.application-info-section.switch.set-value"
				/>
				<Switch
					label={i18n._(REQUIRE_OAUTH2_CODE_GRANT_DESCRIPTOR)}
					description={i18n._(WHEN_ENABLED_INVITING_THIS_BOT_REQUIRES_A_REDIRECT_DESCRIPTOR)}
					value={form.watch('botRequireCodeGrant')}
					onChange={(checked) => form.setValue('botRequireCodeGrant', checked, {shouldDirty: true})}
					data-flx="user.applications-tab.application-detail.application-info-section.switch.set-value--2"
				/>
				<div
					className={styles.redirectList}
					data-flx="user.applications-tab.application-detail.application-info-section.redirect-list"
				>
					{redirectList.map((value, idx) => (
						<div
							key={idx}
							className={styles.redirectRow}
							data-first={idx === 0 ? 'true' : undefined}
							data-flx="user.applications-tab.application-detail.application-info-section.redirect-row"
						>
							<Input
								label={idx === 0 ? i18n._(REDIRECT_URIS_DESCRIPTOR) : undefined}
								name={`redirectUriInputs.${idx}`}
								value={value}
								onChange={(e) => onUpdateRedirect(idx, e.target.value)}
								placeholder={EXAMPLE_CALLBACK_URL}
								error={getRedirectError(idx)}
								data-flx="user.applications-tab.application-detail.application-info-section.input.update-redirect"
							/>
							<div
								className={styles.redirectActions}
								data-flx="user.applications-tab.application-detail.application-info-section.redirect-actions"
							>
								<button
									type="button"
									className={styles.redirectRemoveButton}
									onClick={() => onRemoveRedirect(idx)}
									disabled={idx === 0}
									aria-label={i18n._(DELETE_REDIRECT_URI_DESCRIPTOR)}
									data-flx="user.applications-tab.application-detail.application-info-section.redirect-remove-button.remove-redirect"
								>
									<XIcon
										size={remFromPx(18)}
										weight="bold"
										data-flx="user.applications-tab.application-detail.application-info-section.x-icon"
									/>
								</button>
							</div>
						</div>
					))}
					<Button
						variant="primary"
						fitContent
						className={styles.createRedirectButton}
						onClick={onAddRedirect}
						data-flx="user.applications-tab.application-detail.application-info-section.add-redirect-button"
					>
						{i18n._(ADD_REDIRECT_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		</SectionCard>
	);
};
