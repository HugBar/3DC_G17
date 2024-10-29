import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { StaffService } from '../staff.service';

@Component({
  selector: 'app-create-staff-profile',
  templateUrl: './create-staff-profile.component.html',
  styleUrls: ['./create-staff-profile.component.css']
})
export class CreateStaffProfileComponent {
  staffForm: FormGroup;

  constructor(private fb: FormBuilder, private staffService: StaffService) {
    this.staffForm = this.fb.group({
      email: ['', [Validators.required, Validators.email]],
      phoneNumber: ['', Validators.required],
      // Add other fields as necessary
    });
  }

  onSubmit() {
    if (this.staffForm.valid) {
      this.staffService.createStaffProfile(this.staffForm.value).subscribe(
        response => {
          console.log('Staff profile created successfully', response);
          // Handle success (e.g., navigate to another page or show a success message)
        },
        error => {
          console.error('Error creating staff profile', error);
          // Handle error (e.g., show an error message)
        }
      );
    }
  }
}