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

import {observer} from 'mobx-react-lite';
import React from 'react';
import * as QuickSwitcherActionCreators from '~/actions/QuickSwitcherActionCreators';
import {QuickSwitcherBottomSheet} from '~/components/bottomsheets/QuickSwitcherBottomSheet';
import {Modals} from '~/components/modals/Modals';
import {ContextMenu} from '~/components/uikit/ContextMenu/ContextMenu';
import {Popouts} from '~/components/uikit/Popout/Popouts';
import {Toasts} from '~/components/uikit/Toast/Toasts';
import LayerManager from '~/stores/LayerManager';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import {handleContextMenu} from '~/utils/ContextMenuUtils';

const GlobalOverlays: React.FC = observer(() => {
	const isMobile = MobileLayoutStore.isMobileLayout();
	const quickSwitcherOpen = QuickSwitcherStore.isOpen;

	React.useEffect(() => {
		LayerManager.init();

		document.addEventListener('contextmenu', handleContextMenu, false);
		return () => {
			document.removeEventListener('contextmenu', handleContextMenu, false);
		};
	}, []);

	return (
		<>
			<Modals />
			<Popouts />
			<ContextMenu />
			<Toasts />
			{isMobile && <QuickSwitcherBottomSheet isOpen={quickSwitcherOpen} onClose={QuickSwitcherActionCreators.hide} />}
		</>
	);
});

export default GlobalOverlays;
