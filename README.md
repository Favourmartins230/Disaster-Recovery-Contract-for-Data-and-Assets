# Disaster Recovery Contract for Assets (DRCA)

A Clarity smart contract that enables users to appoint trusted entities for emergency access to their digital assets. This contract provides a secure way to ensure your digital assets can be recovered by designated beneficiaries in case of emergency.

## Features

- Register as a user with a customizable recovery period
- Add and remove trusted beneficiaries (up to 5)
- Store and manage digital assets with metadata
- Initiate recovery process by beneficiaries
- Time-locked recovery to prevent unauthorized access
- Activity tracking to prove user is still active

## Contract Functions

### User Management

- `register(recovery-period)`: Register as a user with a specified recovery period
- `update-activity()`: Update your last active timestamp
- `get-user-info(user)`: Get information about a user

### Beneficiary Management

- `add-beneficiary(beneficiary)`: Add a trusted beneficiary
- `remove-beneficiary(beneficiary)`: Remove a beneficiary
- `is-beneficiary(user, potential-beneficiary)`: Check if an address is a beneficiary
- `get-beneficiary-info(user, beneficiary)`: Get information about a beneficiary relationship

### Asset Management

- `add-asset(asset-id, asset-name, asset-url, asset-data)`: Add a digital asset
- `remove-asset(asset-id)`: Remove a digital asset
- `get-asset(user, asset-id)`: Get information about an asset
- `list-assets(user)`: List all assets for a user

### Recovery Process

- `initiate-recovery(user)`: Initiate the recovery process for a user's assets
- `cancel-recovery(user)`: Cancel a previously initiated recovery process
- `recover-asset(user, asset-id)`: Recover a specific asset after the waiting period
- `can-recover(user, beneficiary)`: Check if recovery conditions are met

## Usage Example

1. Register as a user:
   ```clarity
   (contract-call? .drca register u4320) ;; 3-day recovery period
   ```

2. Add a beneficiary:
   ```clarity
   (contract-call? .drca add-beneficiary 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
   ```

3. Add an asset:
   ```clarity
   (contract-call? .drca add-asset "asset-001" "Private Key" none (some "encrypted-data-here"))
   ```

4. As a beneficiary, initiate recovery:
   ```clarity
   (contract-call? .drca initiate-recovery 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)
   ```

5. After the recovery period, recover the asset:
   ```clarity
   (contract-call? .drca recover-asset 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG "asset-001")
   ```

## Security Considerations

- The recovery period provides a time buffer during which the original owner can cancel the recovery
- Regular activity updates prevent unauthorized recovery attempts
- Limited number of beneficiaries reduces attack surface
- Assets are only accessible after the full recovery period has elapsed

## License

MIT
