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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import {Slider} from '~/components/uikit/Slider';
import AccessibilityStore from '~/stores/AccessibilityStore';
import KeybindStore from '~/stores/KeybindStore';
import {formatKeyCombo} from '~/utils/KeybindUtils';
import {isDesktop} from '~/utils/NativeUtils';

export const ChatFontScalingTabContent: React.FC = observer(() => {
	const fontSize = AccessibilityStore.fontSize;

	return (
		<Slider
			defaultValue={fontSize}
			factoryDefaultValue={16}
			markers={[12, 14, 15, 16, 18, 20, 24]}
			stickToMarkers={true}
			onValueChange={(value) => AccessibilityActionCreators.update({fontSize: value})}
			onMarkerRender={(value) => `${value}px`}
		/>
	);
});

export const AppZoomLevelTabContent: React.FC = observer(() => {
	const zoomLevel = AccessibilityStore.zoomLevel;

	return (
		<Slider
			defaultValue={Math.round(zoomLevel * 100)}
			factoryDefaultValue={100}
			minValue={50}
			maxValue={200}
			step={10}
			markers={[50, 75, 100, 125, 150, 175, 200]}
			stickToMarkers={true}
			onValueChange={(value) => AccessibilityActionCreators.update({zoomLevel: value / 100})}
			onMarkerRender={(value) => `${value}%`}
			onValueRender={(value) => <Trans>{value}%</Trans>}
		/>
	);
});

export type LinguiT = (literals: TemplateStringsArray, ...placeholders: Array<unknown>) => string;

export function getAppZoomLevelDescription(t: LinguiT): string {
	const zoomIn = formatKeyCombo(KeybindStore.keybinds.zoom_in);
	const zoomOut = formatKeyCombo(KeybindStore.keybinds.zoom_out);

	return t`Adjust the overall zoom level of the app. Use ${zoomIn} / ${zoomOut} to adjust quickly.`;
}

export function shouldShowAppZoomLevel(): boolean {
	return isDesktop();
}
