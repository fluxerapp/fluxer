export class ArchiveAttemptSupersededError extends Error {
	constructor() {
		super('Archive attempt no longer owns this archive');
		this.name = 'ArchiveAttemptSupersededError';
	}
}
