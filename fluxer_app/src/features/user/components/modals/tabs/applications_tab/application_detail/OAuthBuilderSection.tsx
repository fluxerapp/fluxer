// SPDX-License-Identifier: AGPL-3.0-or-later

import {SCOPES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import styles from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetail.module.css';
import {SectionCard} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailSectionCard';
import type {ApplicationDetailForm} from '@app/features/user/components/modals/tabs/applications_tab/application_detail/ApplicationDetailTypes';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CopyIcon} from '@phosphor-icons/react';
import type React from 'react';
import {Controller} from 'react-hook-form';

const REDIRECT_URI_IS_REQUIRED_BECAUSE_THIS_BOT_REQUIRES_DESCRIPTOR = msg({
	message: 'Redirect URI is required because this bot requires OAuth2 code grant.',
	comment: 'Description text in the o auth builder section. Keep the tone plain and specific.',
});
const REDIRECT_URI_IS_REQUIRED_WHEN_NOT_USING_ONLY_DESCRIPTOR = msg({
	message: 'Redirect URI is required when not using only the bot scope.',
	comment: 'Description text in the o auth builder section. Keep the tone plain and specific.',
});
const REDIRECT_URI_DESCRIPTOR = msg({
	message: 'Redirect URI',
	comment: 'Short label in the o auth builder section. Keep it concise. Keep the tone plain and specific.',
});
const SELECT_A_REDIRECT_URI_DESCRIPTOR = msg({
	message: 'Select a redirect URI',
	comment:
		'Button or menu action label in the o auth builder section. Keep it concise. Keep the tone plain and specific.',
});
const AUTHORIZE_URL_DESCRIPTOR = msg({
	message: 'Authorize URL',
	comment: 'Short label in the o auth builder section. Keep it concise. Keep the tone plain and specific.',
});
const SELECT_SCOPES_AND_REDIRECT_URI_IF_REQUIRED_DESCRIPTOR = msg({
	message: 'Select scopes (and redirect URI if required)',
	comment:
		'Button or menu action label in the o auth builder section. Keep it concise. Keep the tone plain and specific.',
});
const COPY_AUTHORIZE_URL_DESCRIPTOR = msg({
	message: 'Copy authorize URL',
	comment:
		'Button or menu action label in the o auth builder section. Keep it concise. Keep the tone plain and specific.',
});

interface OAuthBuilderSectionProps {
	form: ApplicationDetailForm;
	availableScopes: ReadonlyArray<string>;
	builderScopeList: Array<string>;
	botPermissionsList: Array<{id: string; label: string}>;
	builderUrl: string;
	redirectOptions: Array<ComboboxOption<string>>;
	onCopyBuilderUrl: () => Promise<void>;
}

export const OAuthBuilderSection: React.FC<OAuthBuilderSectionProps> = ({
	form,
	availableScopes,
	builderScopeList,
	botPermissionsList,
	builderUrl,
	redirectOptions,
	onCopyBuilderUrl,
}) => {
	const {i18n} = useLingui();
	const builderRedirectUri = form.watch('builderRedirectUri');
	const botRequireCodeGrant = form.watch('botRequireCodeGrant') ?? false;
	const isBotOnly = builderScopeList.length === 1 && builderScopeList[0] === 'bot';
	const redirectRequired = builderScopeList.length > 0 && (!isBotOnly || botRequireCodeGrant);
	let redirectError: string | undefined;
	if (redirectRequired && !builderRedirectUri) {
		if (isBotOnly && botRequireCodeGrant) {
			redirectError = i18n._(REDIRECT_URI_IS_REQUIRED_BECAUSE_THIS_BOT_REQUIRES_DESCRIPTOR);
		} else {
			redirectError = i18n._(REDIRECT_URI_IS_REQUIRED_WHEN_NOT_USING_ONLY_DESCRIPTOR);
		}
	}
	return (
		<SectionCard
			title={<Trans>OAuth2 URL builder</Trans>}
			subtitle={<Trans>Construct an authorize URL with scopes and permissions.</Trans>}
			data-flx="user.applications-tab.application-detail.o-auth-builder-section.section-card"
		>
			<div
				className={styles.fieldStack}
				data-flx="user.applications-tab.application-detail.o-auth-builder-section.field-stack"
			>
				<div
					className={styles.scopeGrid}
					data-flx="user.applications-tab.application-detail.o-auth-builder-section.scope-grid"
				>
					<div
						className={styles.fieldLabel}
						data-flx="user.applications-tab.application-detail.o-auth-builder-section.field-label"
					>
						{i18n._(SCOPES_DESCRIPTOR)}
					</div>
					<div
						className={styles.scopeList}
						data-flx="user.applications-tab.application-detail.o-auth-builder-section.scope-list"
					>
						{availableScopes.map((scope) => (
							<div
								key={scope}
								className={styles.scopeItem}
								data-flx="user.applications-tab.application-detail.o-auth-builder-section.scope-item"
							>
								<Controller
									name={`builderScopes.${scope}` as const}
									control={form.control}
									render={({field}) => (
										<Checkbox
											checked={!!field.value}
											onChange={(checked) => field.onChange(checked)}
											size="small"
											data-flx="user.applications-tab.application-detail.o-auth-builder-section.checkbox.change"
										>
											<span
												className={styles.scopeLabel}
												data-flx="user.applications-tab.application-detail.o-auth-builder-section.scope-label"
											>
												{scope}
											</span>
										</Checkbox>
									)}
									data-flx="user.applications-tab.application-detail.o-auth-builder-section.controller"
								/>
							</div>
						))}
					</div>
				</div>
				<Controller
					name="builderRedirectUri"
					control={form.control}
					render={({field}) => (
						<Combobox
							label={i18n._(REDIRECT_URI_DESCRIPTOR)}
							placeholder={i18n._(SELECT_A_REDIRECT_URI_DESCRIPTOR)}
							value={field.value ?? ''}
							options={redirectOptions}
							onChange={(val) => field.onChange(val || '')}
							isClearable
							error={redirectError}
							data-flx="user.applications-tab.application-detail.o-auth-builder-section.select.change"
						/>
					)}
					data-flx="user.applications-tab.application-detail.o-auth-builder-section.controller--2"
				/>
				{builderScopeList.includes('bot') && (
					<div
						className={styles.scopeGrid}
						data-flx="user.applications-tab.application-detail.o-auth-builder-section.scope-grid--2"
					>
						<div
							className={styles.fieldLabel}
							data-flx="user.applications-tab.application-detail.o-auth-builder-section.field-label--2"
						>
							<Trans>Bot permissions</Trans>
						</div>
						<div
							className={`${styles.scopeList} ${styles.botPermissionList}`}
							data-flx="user.applications-tab.application-detail.o-auth-builder-section.scope-list--2"
						>
							{botPermissionsList.map((perm) => (
								<div
									key={perm.id}
									className={styles.scopeItem}
									data-flx="user.applications-tab.application-detail.o-auth-builder-section.scope-item--2"
								>
									<Controller
										name={`builderPermissions.${perm.id}` as const}
										control={form.control}
										render={({field}) => (
											<Checkbox
												checked={!!field.value}
												onChange={(checked) => field.onChange(checked)}
												size="small"
												data-flx="user.applications-tab.application-detail.o-auth-builder-section.checkbox.change--2"
											>
												<span
													className={styles.scopeLabel}
													data-flx="user.applications-tab.application-detail.o-auth-builder-section.scope-label--2"
												>
													{perm.label}
												</span>
											</Checkbox>
										)}
										data-flx="user.applications-tab.application-detail.o-auth-builder-section.controller--3"
									/>
								</div>
							))}
						</div>
					</div>
				)}
				<div
					className={styles.builderResult}
					data-flx="user.applications-tab.application-detail.o-auth-builder-section.builder-result"
				>
					<Input
						label={i18n._(AUTHORIZE_URL_DESCRIPTOR)}
						value={builderUrl}
						readOnly
						placeholder={i18n._(SELECT_SCOPES_AND_REDIRECT_URI_IF_REQUIRED_DESCRIPTOR)}
						rightElement={
							<Button
								variant="primary"
								compact
								fitContent
								aria-label={i18n._(COPY_AUTHORIZE_URL_DESCRIPTOR)}
								leftIcon={
									<CopyIcon
										size={remFromPx(16)}
										data-flx="user.applications-tab.application-detail.o-auth-builder-section.copy-icon"
									/>
								}
								disabled={!builderUrl}
								onClick={onCopyBuilderUrl}
								data-flx="user.applications-tab.application-detail.o-auth-builder-section.button.copy-builder-url"
							>
								<span
									className={styles.srOnly}
									data-flx="user.applications-tab.application-detail.o-auth-builder-section.sr-only"
								>
									<Trans>Copy</Trans>
								</span>
							</Button>
						}
						data-flx="user.applications-tab.application-detail.o-auth-builder-section.input"
					/>
				</div>
			</div>
		</SectionCard>
	);
};
