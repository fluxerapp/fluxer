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

import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import type {PopoutKey} from '~/components/uikit/Popout';

type LayerType = 'modal' | 'popout' | 'contextmenu';

export interface Layer {
	type: LayerType;
	key: string | PopoutKey;
	timestamp: number;
	onClose?: () => void;
}

class LayerManager {
	private layers: Array<Layer> = [];
	private isInitialized = false;

	init() {
		if (this.isInitialized) return;
		this.isInitialized = true;

		document.addEventListener('keydown', this.handleGlobalEscape, {capture: true});
	}

	destroy() {
		if (!this.isInitialized) return;
		this.isInitialized = false;

		document.removeEventListener('keydown', this.handleGlobalEscape, {capture: true});
		this.layers = [];
	}

	private handleGlobalEscape = (event: KeyboardEvent) => {
		if (event.key !== 'Escape') return;

		const topLayer = this.getTopLayer();
		if (!topLayer) return;

		event.preventDefault();
		event.stopPropagation();

		if (topLayer.type === 'modal') {
			if (topLayer.onClose) {
				topLayer.onClose();
			} else {
				ModalActionCreators.pop();
			}
		} else if (topLayer.type === 'popout') {
			topLayer.onClose?.();
			this.removeLayer('popout', topLayer.key);
			PopoutActionCreators.close(topLayer.key);
		} else if (topLayer.type === 'contextmenu') {
			topLayer.onClose?.();
		}
	};

	addLayer(type: LayerType, key: string | PopoutKey, onClose?: () => void) {
		this.removeLayer(type, key);

		this.layers.push({
			type,
			key,
			timestamp: Date.now(),
			onClose,
		});
	}

	removeLayer(type: LayerType, key: string | PopoutKey) {
		this.layers = this.layers.filter((layer) => !(layer.type === type && layer.key === key));
	}

	private getTopLayer(): Layer | undefined {
		return this.layers.length > 0 ? this.layers[this.layers.length - 1] : undefined;
	}

	hasLayers(): boolean {
		return this.layers.length > 0;
	}

	isTopLayer(type: LayerType, key: string | PopoutKey): boolean {
		const topLayer = this.getTopLayer();
		return topLayer?.type === type && topLayer?.key === key;
	}

	hasType(type: LayerType): boolean {
		return this.layers.some((l) => l.type === type);
	}

	isTopType(type: LayerType): boolean {
		const top = this.getTopLayer();
		return top?.type === type;
	}

	closeAll(): void {
		ModalActionCreators.popAll();
		PopoutActionCreators.closeAll();
		this.layers = [];
	}
}

export default new LayerManager();
