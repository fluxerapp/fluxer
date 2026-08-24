// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {PRODUCT_API_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {DeveloperApplication} from '@app/features/devtools/models/DeveloperApplication';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import styles from '@app/features/user/components/modals/tabs/applications_tab/ApplicationsTab.module.css';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {AppWindowIcon, CaretRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface ApplicationsListProps {
	applications: ReadonlyArray<DeveloperApplication>;
	onSelectApplication: (appId: string) => void;
}

const FIRST_APPLICATION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Create your first application to get started with the {apiName}.',
	comment: 'Developer applications empty-state description. apiName is the product API display name.',
});
export const ApplicationsList: React.FC<ApplicationsListProps> = observer(({applications, onSelectApplication}) => {
	const {i18n} = useLingui();
	if (applications.length === 0) {
		return (
			<div className={styles.emptyState} data-flx="user.applications-tab.applications-list.empty-state">
				<StatusSlate
					Icon={AppWindowIcon}
					title={<Trans>No applications yet</Trans>}
					description={i18n._(FIRST_APPLICATION_DESCRIPTION_DESCRIPTOR, {apiName: PRODUCT_API_NAME})}
					data-flx="user.applications-tab.applications-list.status-slate"
				/>
			</div>
		);
	}
	return (
		<div className={styles.listContainer} data-flx="user.applications-tab.applications-list.list-container">
			{applications.map((app) => {
				const avatarUrl = app.bot
					? AvatarUtils.getUserAvatarURL({id: app.bot.id, avatar: app.bot.avatar}, false)
					: null;
				const createdAt = DateUtils.getFormattedShortDate(SnowflakeUtils.extractTimestamp(app.id));
				return (
					<div
						key={app.id}
						className={styles.itemContainer}
						data-flx="user.applications-tab.applications-list.item-container"
					>
						<FocusRing offset={-2} data-flx="user.applications-tab.applications-list.focus-ring">
							<button
								type="button"
								className={styles.itemButton}
								onClick={() => onSelectApplication(app.id)}
								data-flx="user.applications-tab.applications-list.item-button.select-application"
							>
								<div className={styles.itemLeft} data-flx="user.applications-tab.applications-list.item-left">
									{avatarUrl ? (
										<img
											src={avatarUrl}
											alt=""
											className={styles.itemAvatar}
											draggable={false}
											data-flx="user.applications-tab.applications-list.item-avatar"
										/>
									) : (
										<div
											className={styles.itemAvatarPlaceholder}
											aria-hidden
											data-flx="user.applications-tab.applications-list.item-avatar-placeholder"
										>
											{app.name.charAt(0).toUpperCase()}
										</div>
									)}
									<div
										className={styles.itemTextBlock}
										data-flx="user.applications-tab.applications-list.item-text-block"
									>
										<div
											className={styles.itemTitleRow}
											data-flx="user.applications-tab.applications-list.item-title-row"
										>
											<span className={styles.itemName} data-flx="user.applications-tab.applications-list.item-name">
												{app.name}
											</span>
										</div>
										<div
											className={styles.itemMetaRow}
											data-flx="user.applications-tab.applications-list.item-meta-row"
										>
											<span data-flx="user.applications-tab.applications-list.span">
												<Trans>Created {createdAt}</Trans>
											</span>
										</div>
									</div>
								</div>
								<CaretRightIcon
									className={styles.itemChevron}
									weight="bold"
									data-flx="user.applications-tab.applications-list.item-chevron"
								/>
							</button>
						</FocusRing>
					</div>
				);
			})}
		</div>
	);
});
