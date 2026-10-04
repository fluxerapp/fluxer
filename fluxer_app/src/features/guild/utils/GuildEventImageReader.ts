// SPDX-License-Identifier: AGPL-3.0-or-later

interface GuildEventImageReaderCallbacks {
	onLoad: (image: string) => void;
	onError: (error: Error) => void;
	onPending: (pending: boolean) => void;
}

/** At most one selected image may write back into the current event draft. */
export class GuildEventImageReader {
	private reader: FileReader | null = null;

	constructor(private readonly callbacks: GuildEventImageReaderCallbacks) {}

	read(file: File): void {
		this.cancel();
		const reader = new FileReader();
		this.reader = reader;
		const finish = (apply: () => void) => {
			if (this.reader !== reader) return;
			this.reader = null;
			this.detach(reader);
			this.callbacks.onPending(false);
			apply();
		};
		reader.onload = () => {
			finish(() => {
				if (typeof reader.result === 'string') this.callbacks.onLoad(reader.result);
				else this.callbacks.onError(new Error('Unable to read image'));
			});
		};
		reader.onerror = () => finish(() => this.callbacks.onError(reader.error ?? new Error('Unable to read image')));
		reader.onabort = () => finish(() => {});
		this.callbacks.onPending(true);
		try {
			reader.readAsDataURL(file);
		} catch (cause) {
			finish(() => this.callbacks.onError(cause instanceof Error ? cause : new Error('Unable to read image')));
		}
	}

	cancel(notify = true): void {
		const reader = this.reader;
		if (!reader) return;
		this.reader = null;
		this.detach(reader);
		if (reader.readyState === FileReader.LOADING) reader.abort();
		if (notify) this.callbacks.onPending(false);
	}

	private detach(reader: FileReader): void {
		reader.onload = null;
		reader.onerror = null;
		reader.onabort = null;
	}
}
