// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import type {PopoutKey} from '@app/features/ui/popover';

type LayerType = 'modal' | 'popout' | 'contextmenu';

export interface Layer {
	type: LayerType;
	key: string | PopoutKey;
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
		if (topLayer.type !== 'popout' && !topLayer.onClose) return;
		event.preventDefault();
		event.stopImmediatePropagation();
		if (topLayer.type === 'popout') {
			topLayer.onClose?.();
			this.removeLayer('popout', topLayer.key);
			PopoutCommands.close(topLayer.key);
			return;
		}
		topLayer.onClose?.();
	};

	addLayer(type: LayerType, key: string | PopoutKey, onClose?: () => void) {
		this.removeLayer(type, key);
		this.layers.push({
			type,
			key,
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
		ModalCommands.popAll();
		PopoutCommands.closeAll();
		this.layers = [];
	}
}

export default new LayerManager();
