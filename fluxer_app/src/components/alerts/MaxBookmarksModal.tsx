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

import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {modal, push} from '~/actions/ModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {PremiumModal} from '~/components/modals/PremiumModal';
import UserStore from '~/stores/UserStore';

export const MaxBookmarksModal = observer(() => {
	const {t} = useLingui();
	const currentUser = UserStore.currentUser!;
	const isPremium = currentUser.isPremium();
	const maxBookmarks = currentUser.maxBookmarks;

	if (isPremium) {
		return (
			<ConfirmModal
				title={t`Bookmark Limit Reached`}
				description={
					<Trans>
						You've reached the maximum number of bookmarks (
						<Plural value={maxBookmarks} one="# bookmark" other="# bookmarks" />
						). Please remove some bookmarks before adding new ones.
					</Trans>
				}
				primaryText={t`Understood`}
				onPrimary={() => {}}
			/>
		);
	}

	return (
		<ConfirmModal
			title={t`Bookmark Limit Reached`}
			description={
				<Trans>
					You've reached the maximum number of bookmarks for free users (
					<Plural value={maxBookmarks} one="# bookmark" other="# bookmarks" />
					). Upgrade to Plutonium to increase your limit to 300 bookmarks, or remove some bookmarks to add new ones.
				</Trans>
			}
			primaryText={t`Upgrade to Plutonium`}
			primaryVariant="primary"
			onPrimary={() => push(modal(() => <PremiumModal />))}
			secondaryText={t`Dismiss`}
		/>
	);
});
