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

import {makeAutoObservable} from 'mobx';

const BASE_Z_INDEX = 10000;
const Z_INDEX_INCREMENT = 10;

class OverlayStackStore {
	private counter = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	acquire(): number {
		const zIndex = BASE_Z_INDEX + this.counter * Z_INDEX_INCREMENT;
		this.counter++;
		return zIndex;
	}

	release(): void {
		if (this.counter > 0) {
			this.counter--;
		}
	}

	peek(): number {
		return BASE_Z_INDEX + this.counter * Z_INDEX_INCREMENT;
	}

	reset(): void {
		this.counter = 0;
	}
}

export default new OverlayStackStore();
