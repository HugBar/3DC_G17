import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class StaffService {
  private apiUrl = '/api/staff/create-staff-profile';

  constructor(private http: HttpClient) {}

  createStaffProfile(staffData: any): Observable<any> {
    return this.http.post(this.apiUrl, staffData);
  }
}