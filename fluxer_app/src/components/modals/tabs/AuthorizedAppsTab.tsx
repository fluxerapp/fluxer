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

import {t} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {AppWindowIcon, CaretDownIcon, NetworkSlashIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import type {OAuth2Authorization} from '~/actions/OAuth2AuthorizationActionCreators';
import * as OAuth2AuthorizationActionCreators from '~/actions/OAuth2AuthorizationActionCreators';
import {getOAuth2ScopeDescription, type OAuth2Scope} from '~/Constants';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {getUserAvatarURL} from '~/utils/AvatarUtils';
import styles from './AuthorizedAppsTab.module.css';

const formatDate = (dateString: string): string => {
	const date = new Date(dateString);
	return date.toLocaleDateString(undefined, {
		year: 'numeric',
		month: 'short',
		day: 'numeric',
	});
};

const AuthorizedAppsTab = observer(function AuthorizedAppsTab() {
	const {i18n} = useLingui();

	const [authorizations, setAuthorizations] = React.useState<Array<OAuth2Authorization>>([]);
	const [loading, setLoading] = React.useState(true);
	const [error, setError] = React.useState<string | null>(null);
	const [expandedIds, setExpandedIds] = React.useState<Set<string>>(new Set());

	const toggleExpanded = React.useCallback((appId: string) => {
		setExpandedIds((prev) => {
			const next = new Set(prev);
			if (next.has(appId)) {
				next.delete(appId);
			} else {
				next.add(appId);
			}
			return next;
		});
	}, []);

	const loadAuthorizations = React.useCallback(async () => {
		setLoading(true);
		setError(null);

		try {
			const data = await OAuth2AuthorizationActionCreators.listAuthorizations();
			setAuthorizations(data);
		} catch (_err) {
			setError(t`Failed to load authorized applications`);
		} finally {
			setLoading(false);
		}
	}, []);

	React.useEffect(() => {
		loadAuthorizations();
	}, [loadAuthorizations]);

	const handleDeauthorize = React.useCallback((authorization: OAuth2Authorization) => {
		const appName = authorization.application.name;

		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Deauthorize Application`}
					description={t`Are you sure you want to deauthorize ${appName}? This application will no longer have access to your account.`}
					primaryText={t`Deauthorize`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						await OAuth2AuthorizationActionCreators.deauthorize(authorization.application.id);
						setAuthorizations((prev) => prev.filter((a) => a.application.id !== authorization.application.id));
					}}
				/>
			)),
		);
	}, []);

	if (loading) {
		return (
			<div className={styles.loadingContainer}>
				<Spinner size="large" />
			</div>
		);
	}

	if (error) {
		return (
			<StatusSlate
				Icon={NetworkSlashIcon}
				title={t`Failed to load authorized applications`}
				description={error}
				actions={[
					{
						text: t`Retry`,
						onClick: loadAuthorizations,
						variant: 'primary',
					},
				]}
			/>
		);
	}

	if (authorizations.length === 0) {
		return (
			<StatusSlate
				Icon={AppWindowIcon}
				title={<Trans>No Authorized Applications</Trans>}
				description={<Trans>You haven&apos;t authorized any applications to access your account.</Trans>}
				fullHeight
			/>
		);
	}

	return (
		<div className={styles.container}>
			<div className={styles.header}>
				<h2 className={styles.title}>
					<Trans>Authorized Applications</Trans>
				</h2>
				<p className={styles.description}>
					<Trans>These applications have been granted access to your Fluxer account.</Trans>
				</p>
			</div>

			<div className={styles.scrollContainer}>
				<Scroller className={styles.scrollerPadding}>
					<div className={styles.appList}>
						{authorizations.map((authorization) => {
							const iconUrl = authorization.application.icon
								? getUserAvatarURL({
										id: authorization.application.id,
										avatar: authorization.application.icon,
									})
								: null;

							const isExpanded = expandedIds.has(authorization.application.id);
							const authorizedOn = formatDate(authorization.authorized_at);

							return (
								<div key={authorization.application.id} className={styles.appCard}>
									<button
										type="button"
										className={styles.headerButton}
										onClick={() => toggleExpanded(authorization.application.id)}
										aria-expanded={isExpanded}
									>
										<div className={styles.left}>
											<div className={styles.appAvatar} aria-hidden>
												{iconUrl ? (
													<img src={iconUrl} alt={authorization.application.name} className={styles.appAvatarImage} />
												) : (
													<AppWindowIcon className={styles.appAvatarPlaceholder} />
												)}
											</div>

											<div className={styles.textBlock}>
												<div className={styles.titleRow}>
													<span className={styles.appName}>{authorization.application.name}</span>
												</div>
												<div className={styles.metaRow}>
													<span className={styles.metaText}>
														<Trans>Authorized on {authorizedOn}</Trans>
													</span>
												</div>
											</div>
										</div>

										<CaretDownIcon
											weight="bold"
											className={clsx(styles.chevron, isExpanded && styles.chevronExpanded)}
										/>
									</button>

									{isExpanded && (
										<div className={styles.details}>
											<div className={styles.detailsRow}>
												<div className={styles.scopeColumn}>
													<div className={styles.sectionLabel}>
														<Trans>Permissions granted</Trans>
													</div>

													<div className={styles.scopeList}>
														{authorization.scopes.map((scope) => (
															<div key={scope} className={styles.scopeTag}>
																<span className={styles.scopeName}>{scope}</span>
																<span className={styles.scopeDescription}>
																	{getOAuth2ScopeDescription(i18n, scope as OAuth2Scope) || scope}
																</span>
															</div>
														))}
													</div>
												</div>

												<div className={styles.actions}>
													<Button variant="danger-primary" small onClick={() => handleDeauthorize(authorization)}>
														<Trans>Deauthorize</Trans>
													</Button>
												</div>
											</div>
										</div>
									)}
								</div>
							);
						})}
					</div>
				</Scroller>
			</div>
		</div>
	);
});

export default AuthorizedAppsTab;
