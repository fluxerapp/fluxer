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
import type React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import {Switch} from '~/components/form/Switch';
import AccessibilityStore from '~/stores/AccessibilityStore';

export const FavoritesTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	return (
		<Switch
			label={t`Enable Favorites`}
			description={t`When enabled, you can favorite channels and they'll appear in the Favorites section. When disabled, all favorite-related UI elements (buttons, menu items) will be hidden. Your existing favorites will be preserved.`}
			value={AccessibilityStore.showFavorites}
			onChange={(value) => AccessibilityActionCreators.update({showFavorites: value})}
		/>
	);
});
