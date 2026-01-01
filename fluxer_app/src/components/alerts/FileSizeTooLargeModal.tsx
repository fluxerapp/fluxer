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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import UserStore from '~/stores/UserStore';

export const FileSizeTooLargeModal = observer(() => {
	const {t} = useLingui();
	const user = UserStore.currentUser;
	const hasPremium = user?.isPremium() ?? false;

	if (hasPremium) {
		return (
			<ConfirmModal
				title={t`File size too large`}
				description={t`The file you're trying to upload exceeds the maximum size limit of 500 MB for Plutonium subscribers.`}
				primaryText={t`Understood`}
				onPrimary={() => {}}
			/>
		);
	}

	return (
		<ConfirmModal
			title={t`File Size Limit Exceeded`}
			description={t`The file you're trying to upload exceeds the maximum size limit of 25 MB for non-subscribers. With Plutonium, you can upload files up to 500 MB, use animated avatars and banners, write longer bios, and unlock many other premium features.`}
			primaryText={t`Get Plutonium`}
			primaryVariant="primary"
			onPrimary={() => PremiumModalActionCreators.open()}
			secondaryText={t`Cancel`}
		/>
	);
});
