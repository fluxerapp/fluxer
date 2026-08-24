// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {PREMIUM_PRODUCT_FULL_NAME, PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {CreatePackModal} from '@app/features/expressions/components/modals/CreatePackModal';
import {EditPackModal} from '@app/features/expressions/components/modals/EditPackModal';
import {PackInviteModal} from '@app/features/expressions/components/modals/PackInviteModal';
import Packs from '@app/features/expressions/state/ExpressionsPacks';
import {NO_DESCRIPTION_PROVIDED_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import styles from '@app/features/user/components/modals/tabs/ExpressionPacksTab.module.css';
import Users from '@app/features/user/state/Users';
import {getFormattedShortDate} from '@app/features/user/utils/DateFormatting';
import type {PackSummaryResponse} from '@fluxer/schema/src/domains/pack/PackSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {StickerIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

const UNLIMITED_DESCRIPTOR = msg({
	message: 'Unlimited',
	comment: 'Short label in the expression packs tab. Keep it concise.',
});
const UNABLE_TO_LOAD_PACK_INFORMATION_DESCRIPTOR = msg({
	message: 'Unable to load pack information.',
	comment: 'Error message in the expression packs tab.',
});
const DELETE_PACK_DESCRIPTOR = msg({
	message: 'Delete pack',
	comment:
		'Button or menu action label in the expression packs tab. Keep it concise. Keep the tone plain and specific.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: "Delete this pack? Can't be undone.",
	comment: 'Error message in the expression packs tab. Keep the tone plain and specific.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment:
		'Button or menu action label in the expression packs tab. Keep it concise. Keep the tone plain and specific.',
});
const REMOVE_PACK_DESCRIPTOR = msg({
	message: 'Remove pack',
	comment:
		'Button or menu action label in the expression packs tab. Keep it concise. Keep the tone plain and specific.',
});
const EXPRESSION_PACKS_PREMIUM_FEATURE_DESCRIPTOR = msg({
	message: 'Expression packs are a {premiumProductName} feature',
	comment: 'Empty-state title for expression packs when the feature requires premium.',
});
const PREMIUM_EXPRESSION_PACKS_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Create and share custom emoji and sticker packs with {premiumProductFullName}.',
	comment: 'Empty-state description for expression packs when the feature requires premium.',
});
const LEARN_ABOUT_PREMIUM_DESCRIPTOR = msg({
	message: 'Learn about {premiumProductName}',
	comment: 'CTA label that opens the premium settings tab from expression packs.',
});
const REMOVING_THE_PACK_WILL_UNINSTALL_IT_FROM_YOUR_DESCRIPTOR = msg({
	message: 'Removing the pack will uninstall it from your account.',
	comment: 'Description text in the expression packs tab.',
});
const REMOVE_DESCRIPTOR = msg({
	message: 'Remove',
	comment:
		'Button or menu action label in the expression packs tab. Keep it concise. Keep the tone plain and specific.',
});
const EMOJI_PACKS_DESCRIPTOR = msg({
	message: 'Emoji packs',
	comment: 'Short label in the expression packs tab. Keep it concise.',
});
const STICKER_PACKS_DESCRIPTOR = msg({
	message: 'Sticker packs',
	comment: 'Short label in the expression packs tab. Keep it concise.',
});
const PACK_TYPES: Array<{key: 'emoji' | 'sticker'}> = [{key: 'emoji'}, {key: 'sticker'}];
const PackCard: React.FC<{
	pack: PackSummaryResponse;
	onUninstall?: () => void;
	onEdit?: () => void;
	onInvite?: () => void;
	created?: boolean;
}> = observer(({pack, onUninstall, onEdit, onInvite, created}) => {
	const {i18n} = useLingui();
	const installedAt = pack.installed_at ? getFormattedShortDate(new Date(pack.installed_at)) : null;
	return (
		<div className={styles.packCard} data-flx="user.expression-packs-tab.pack-card.pack-card">
			<div className={styles.packCardHeader} data-flx="user.expression-packs-tab.pack-card.pack-card-header">
				<h3 className={styles.packName} data-flx="user.expression-packs-tab.pack-card.pack-name">
					{pack.name}
				</h3>
				<span className={styles.packMeta} data-flx="user.expression-packs-tab.pack-card.pack-meta">
					{pack.type === 'emoji' ? <Trans>Emoji pack</Trans> : <Trans>Sticker pack</Trans>}
				</span>
			</div>
			<p className={styles.packDescription} data-flx="user.expression-packs-tab.pack-card.pack-description">
				{pack.description || i18n._(NO_DESCRIPTION_PROVIDED_DESCRIPTOR)}
			</p>
			{installedAt && (
				<p className={styles.packTimestamp} data-flx="user.expression-packs-tab.pack-card.pack-timestamp">
					<Trans>Installed on {installedAt}</Trans>
				</p>
			)}
			<div className={styles.cardActions} data-flx="user.expression-packs-tab.pack-card.card-actions">
				{created && (
					<>
						<Button variant="secondary" onClick={onInvite} data-flx="user.expression-packs-tab.pack-card.button.invite">
							<Trans>Invite</Trans>
						</Button>
						<Button variant="secondary" onClick={onEdit} data-flx="user.expression-packs-tab.pack-card.button.edit">
							<Trans>Edit</Trans>
						</Button>
					</>
				)}
				{onUninstall && (
					<Button
						variant="danger"
						onClick={onUninstall}
						data-flx="user.expression-packs-tab.pack-card.button.uninstall"
					>
						<Trans>Remove</Trans>
					</Button>
				)}
			</div>
		</div>
	);
});
const ExpressionPacksTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const formatLimit = (value: number): string => {
		if (value === Number.POSITIVE_INFINITY) return i18n._(UNLIMITED_DESCRIPTOR);
		return value.toString();
	};
	const currentUser = Users.currentUser;
	const hasGlobalExpressions = useMemo(
		() =>
			isLimitToggleEnabled(
				{feature_global_expressions: LimitResolver.resolve({key: 'feature_global_expressions', fallback: 0})},
				'feature_global_expressions',
			),
		[],
	);
	const [loaded, setLoaded] = useState(false);
	useEffect(() => {
		if (!hasGlobalExpressions || loaded) return;
		Packs.fetch().finally(() => setLoaded(true));
	}, [hasGlobalExpressions, loaded]);
	if (!currentUser) return null;
	if (!hasGlobalExpressions && RuntimeConfig.isSelfHosted()) {
		return (
			<div className={styles.emptyState} data-flx="user.expression-packs-tab.empty-state">
				<StatusSlate
					Icon={StickerIcon}
					title={<Trans>Expression packs</Trans>}
					description={
						<Trans>
							Expression packs are not enabled on this instance. Contact your instance administrator for more
							information.
						</Trans>
					}
					data-flx="user.expression-packs-tab.status-slate"
				/>
			</div>
		);
	}
	if (!hasGlobalExpressions) {
		return (
			<div className={styles.emptyState} data-flx="user.expression-packs-tab.empty-state--2">
				<StatusSlate
					Icon={StickerIcon}
					title={i18n._(EXPRESSION_PACKS_PREMIUM_FEATURE_DESCRIPTOR, {
						premiumProductName: PREMIUM_PRODUCT_NAME,
					})}
					description={i18n._(PREMIUM_EXPRESSION_PACKS_DESCRIPTION_DESCRIPTOR, {
						premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME,
					})}
					actions={[
						{
							text: i18n._(LEARN_ABOUT_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME}),
							onClick: () => ComponentBus.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'plutonium'}),
							variant: 'primary',
							fitContent: true,
						},
					]}
					data-flx="user.expression-packs-tab.status-slate--2"
				/>
			</div>
		);
	}
	const dashboard = Packs.dashboard;
	const fetchStatus = Packs.fetchStatus;
	if (fetchStatus === 'pending') {
		return (
			<div className={styles.spinnerWrapper} data-flx="user.expression-packs-tab.spinner-wrapper">
				<Spinner data-flx="user.expression-packs-tab.spinner" />
			</div>
		);
	}
	if (!dashboard) {
		return (
			<div className={styles.emptyState} data-flx="user.expression-packs-tab.empty-state--3">
				<p data-flx="user.expression-packs-tab.p">{i18n._(UNABLE_TO_LOAD_PACK_INFORMATION_DESCRIPTOR)}</p>
			</div>
		);
	}
	const handleOpenCreate = (type: 'emoji' | 'sticker') => {
		ModalCommands.push(
			modal(() => (
				<CreatePackModal
					type={type}
					onSuccess={() => Packs.fetch()}
					data-flx="user.expression-packs-tab.handle-open-create.create-pack-modal"
				/>
			)),
		);
	};
	const handleOpenEdit = (pack: PackSummaryResponse) => {
		ModalCommands.push(
			modal(() => (
				<EditPackModal
					packId={pack.id}
					type={pack.type}
					name={pack.name}
					description={pack.description}
					onSuccess={() => Packs.fetch()}
					data-flx="user.expression-packs-tab.handle-open-edit.edit-pack-modal"
				/>
			)),
		);
	};
	const handleOpenInvite = (pack: PackSummaryResponse) => {
		ModalCommands.push(
			modal(() => (
				<PackInviteModal
					packId={pack.id}
					type={pack.type}
					onCreated={() => Packs.fetch()}
					data-flx="user.expression-packs-tab.handle-open-invite.pack-invite-modal"
				/>
			)),
		);
	};
	const handleDelete = (packId: string) => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_PACK_DESCRIPTOR)}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR)}
					primaryText={i18n._(DELETE_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						await Packs.deletePack(packId);
					}}
					data-flx="user.expression-packs-tab.handle-delete.confirm-modal"
				/>
			)),
		);
	};
	const handleUninstall = (packId: string) => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(REMOVE_PACK_DESCRIPTOR)}
					description={i18n._(REMOVING_THE_PACK_WILL_UNINSTALL_IT_FROM_YOUR_DESCRIPTOR)}
					primaryText={i18n._(REMOVE_DESCRIPTOR)}
					onPrimary={async () => {
						await Packs.uninstallPack(packId);
					}}
					data-flx="user.expression-packs-tab.handle-uninstall.confirm-modal"
				/>
			)),
		);
	};
	return (
		<SettingsTabContainer data-flx="user.expression-packs-tab.settings-tab-container">
			{PACK_TYPES.map((section) => {
				const data = section.key === 'emoji' ? dashboard.emoji : dashboard.sticker;
				const sectionId = section.key === 'emoji' ? 'emoji-packs' : 'sticker-packs';
				const sectionLabel =
					section.key === 'emoji' ? i18n._(EMOJI_PACKS_DESCRIPTOR) : i18n._(STICKER_PACKS_DESCRIPTOR);
				const installedCount = data.installed.length;
				const installedLimit = formatLimit(data.installed_limit);
				const createdCount = data.created.length;
				const createdLimit = formatLimit(data.created_limit);
				return (
					<SettingsSection
						key={section.key}
						id={sectionId}
						title={sectionLabel}
						description={
							<Trans>
								Installed {installedCount} / {installedLimit}
							</Trans>
						}
						data-flx="user.expression-packs-tab.section"
					>
						<div className={styles.sectionActions} data-flx="user.expression-packs-tab.section-actions">
							<Button
								onClick={() => handleOpenCreate(section.key)}
								data-flx="user.expression-packs-tab.button.open-create"
							>
								{section.key === 'emoji' ? <Trans>Create emoji pack</Trans> : <Trans>Create sticker pack</Trans>}
							</Button>
						</div>
						<div className={styles.listWrapper} data-flx="user.expression-packs-tab.list-wrapper">
							{data.installed.length === 0 && (
								<p className={styles.emptyText} data-flx="user.expression-packs-tab.empty-text">
									<Trans>No installed packs yet.</Trans>
								</p>
							)}
							{data.installed.map((pack) => (
								<PackCard
									key={pack.id}
									pack={pack}
									onUninstall={() => handleUninstall(pack.id)}
									data-flx="user.expression-packs-tab.pack-card"
								/>
							))}
						</div>
						<SettingsTabSection
							title={
								<Trans>
									Created {createdCount} / {createdLimit}
								</Trans>
							}
							data-flx="user.expression-packs-tab.created-section"
						>
							<div className={styles.listWrapper} data-flx="user.expression-packs-tab.list-wrapper--2">
								{data.created.length === 0 && (
									<p className={styles.emptyText} data-flx="user.expression-packs-tab.empty-text--2">
										<Trans>You haven't created any packs yet.</Trans>
									</p>
								)}
								{data.created.map((pack) => (
									<PackCard
										key={pack.id}
										pack={pack}
										created={true}
										onInvite={() => handleOpenInvite(pack)}
										onEdit={() => handleOpenEdit(pack)}
										onUninstall={() => handleDelete(pack.id)}
										data-flx="user.expression-packs-tab.pack-card--2"
									/>
								))}
							</div>
						</SettingsTabSection>
					</SettingsSection>
				);
			})}
		</SettingsTabContainer>
	);
});

export default ExpressionPacksTab;
