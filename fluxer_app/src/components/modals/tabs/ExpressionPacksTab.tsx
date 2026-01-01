/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {StickerIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {CreatePackModal} from '~/components/modals/CreatePackModal';
import {EditPackModal} from '~/components/modals/EditPackModal';
import {PackInviteModal} from '~/components/modals/PackInviteModal';
import {
	SettingsTabContainer,
	SettingsTabHeader,
	SettingsTabSection,
} from '~/components/modals/shared/SettingsTabLayout';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import PackStore from '~/stores/PackStore';
import UserStore from '~/stores/UserStore';
import type {PackSummary} from '~/types/PackTypes';
import {getFormattedShortDate} from '~/utils/DateUtils';
import styles from './ExpressionPacksTab.module.css';

const PACK_TYPES: Array<{key: 'emoji' | 'sticker'; label: string}> = [
	{key: 'emoji', label: 'Emoji packs'},
	{key: 'sticker', label: 'Sticker packs'},
];

const formatLimit = (value: number): string => {
	if (value === Number.POSITIVE_INFINITY) return 'Unlimited';
	return value.toString();
};

const PackCard: React.FC<{
	pack: PackSummary;
	onUninstall?: () => void;
	onEdit?: () => void;
	onInvite?: () => void;
	created?: boolean;
}> = observer(({pack, onUninstall, onEdit, onInvite, created}) => {
	const {t} = useLingui();
	const installedAt = pack.installed_at ? getFormattedShortDate(new Date(pack.installed_at)) : null;

	return (
		<div className={styles.packCard}>
			<div className={styles.packCardHeader}>
				<h3 className={styles.packName}>{pack.name}</h3>
				<span className={styles.packMeta}>
					{pack.type === 'emoji' ? <Trans>Emoji pack</Trans> : <Trans>Sticker pack</Trans>}
				</span>
			</div>
			<p className={styles.packDescription}>{pack.description || t`No description provided.`}</p>
			{installedAt && (
				<p className={styles.packTimestamp}>
					<Trans>Installed on {installedAt}</Trans>
				</p>
			)}
			<div className={styles.cardActions}>
				{created && (
					<>
						<Button variant="secondary" onClick={onInvite}>
							<Trans>Invite</Trans>
						</Button>
						<Button variant="secondary" onClick={onEdit}>
							<Trans>Edit</Trans>
						</Button>
					</>
				)}
				{onUninstall && (
					<Button variant="danger-secondary" onClick={onUninstall}>
						<Trans>Remove</Trans>
					</Button>
				)}
			</div>
		</div>
	);
});

const ExpressionPacksTab: React.FC = observer(() => {
	const {t} = useLingui();
	const currentUser = UserStore.currentUser;
	const hasPremium = currentUser?.isPremium() ?? false;
	const [loaded, setLoaded] = React.useState(false);

	React.useEffect(() => {
		if (!hasPremium || loaded) return;
		PackStore.fetch().finally(() => setLoaded(true));
	}, [hasPremium, loaded]);

	if (!currentUser) return null;

	if (!hasPremium) {
		return (
			<div className={styles.emptyState}>
				<StatusSlate
					Icon={StickerIcon}
					title={<Trans>Expression Packs are a Plutonium feature</Trans>}
					description={<Trans>Create and share custom emoji and sticker packs with Fluxer Plutonium.</Trans>}
					actions={[
						{
							text: <Trans>Learn about Plutonium</Trans>,
							onClick: () => ComponentDispatch.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'plutonium'}),
							variant: 'primary',
							fitContent: true,
						},
					]}
				/>
			</div>
		);
	}

	const dashboard = PackStore.dashboard;
	const fetchStatus = PackStore.fetchStatus;

	if (fetchStatus === 'pending') {
		return (
			<div className={styles.spinnerWrapper}>
				<Spinner />
			</div>
		);
	}

	if (!dashboard) {
		return (
			<div className={styles.emptyState}>
				<p>{t`Unable to load pack information.`}</p>
			</div>
		);
	}

	const handleOpenCreate = (type: 'emoji' | 'sticker') => {
		ModalActionCreators.push(modal(() => <CreatePackModal type={type} onSuccess={() => PackStore.fetch()} />));
	};

	const handleOpenEdit = (pack: PackSummary) => {
		ModalActionCreators.push(
			modal(() => (
				<EditPackModal
					packId={pack.id}
					type={pack.type}
					name={pack.name}
					description={pack.description}
					onSuccess={() => PackStore.fetch()}
				/>
			)),
		);
	};

	const handleOpenInvite = (pack: PackSummary) => {
		ModalActionCreators.push(
			modal(() => <PackInviteModal packId={pack.id} type={pack.type} onCreated={() => PackStore.fetch()} />),
		);
	};

	const handleDelete = (packId: string) => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Delete pack`}
					description={t`Are you sure you want to delete this pack? This cannot be undone.`}
					primaryText={t`Delete`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						await PackStore.deletePack(packId);
					}}
				/>
			)),
		);
	};

	const handleUninstall = (packId: string) => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Remove pack`}
					description={t`Removing the pack will uninstall it from your account.`}
					primaryText={t`Remove`}
					onPrimary={async () => {
						await PackStore.uninstallPack(packId);
					}}
				/>
			)),
		);
	};

	return (
		<SettingsTabContainer>
			<SettingsTabHeader
				title={t`Expression Packs`}
				description={<Trans>Manage emoji and sticker packs that you've created or installed.</Trans>}
			/>
			{PACK_TYPES.map((section) => {
				const data = section.key === 'emoji' ? dashboard.emoji : dashboard.sticker;

				return (
					<SettingsTabSection key={section.key} className={styles.section}>
						<div className={styles.sectionHeader}>
							<div>
								<h2 className={styles.sectionTitle}>{section.label}</h2>
								<p className={styles.sectionSubtitle}>
									<Trans>
										Installed {data.installed.length} / {formatLimit(data.installed_limit)}
									</Trans>
								</p>
							</div>
							<Button onClick={() => handleOpenCreate(section.key)}>
								{section.key === 'emoji' ? <Trans>Create Emoji Pack</Trans> : <Trans>Create Sticker Pack</Trans>}
							</Button>
						</div>

						<div className={styles.listWrapper}>
							{data.installed.length === 0 && (
								<p className={styles.emptyText}>
									<Trans>No installed packs yet.</Trans>
								</p>
							)}
							{data.installed.map((pack) => (
								<PackCard key={pack.id} pack={pack} onUninstall={() => handleUninstall(pack.id)} />
							))}
						</div>

						<div className={styles.sectionHeader}>
							<div>
								<h3 className={styles.sectionSubtitle}>
									<Trans>
										Created {data.created.length} / {formatLimit(data.created_limit)}
									</Trans>
								</h3>
							</div>
						</div>

						<div className={styles.listWrapper}>
							{data.created.length === 0 && (
								<p className={styles.emptyText}>
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
								/>
							))}
						</div>
					</SettingsTabSection>
				);
			})}
		</SettingsTabContainer>
	);
});

export default ExpressionPacksTab;
